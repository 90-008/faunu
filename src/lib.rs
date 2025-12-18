use async_lock::{RwLock, RwLockReadGuard, RwLockUpgradableReadGuard, RwLockWriteGuard};
use futures::FutureExt;
use js_sys::Promise;
use miette::Report;
use nu_cmd_base::hook::eval_hook;
use nu_cmd_extra::add_extra_command_context;
use nu_cmd_lang::create_default_context;
use nu_engine::{command_prelude::*, eval_block};
use nu_parser::{FlatShape, flatten_block, parse};
use nu_protocol::{
    Config, ListStream, PipelineData, Signals, Span,
    engine::{EngineState, Stack, StateWorkingSet},
};
use std::{
    io::Cursor,
    sync::{Arc, OnceLock},
};
use wasm_bindgen::prelude::*;
use wasm_bindgen_futures::future_to_promise;

pub mod cmd;
pub mod completion;
pub mod default_context;
pub mod error;
pub mod globals;
pub mod highlight;
pub mod memory_fs;

use crate::{
    cmd::{
        Cd, Fetch, Job, JobKill, JobList, Ls, Mkdir, Mv, Open, Print, Pwd, Random, Rm, Save, Source, Sys,
        source::eval_file,
    },
    default_context::add_shell_command_context,
    error::format_error,
    globals::{
        InterruptBool, apply_pending_deltas, get_pwd, get_vfs, print_to_console, set_interrupt,
    },
};
use error::CommandError;

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn error(msg: String);

    type Error;

    #[wasm_bindgen(constructor)]
    fn new() -> Error;

    #[wasm_bindgen(structural, method, getter)]
    fn stack(error: &Error) -> String;
}

fn panic_hook(info: &std::panic::PanicHookInfo) {
    let mut msg = info.to_string();

    msg.push_str("\n\nStack:\n\n");
    let e = Error::new();
    let stack = e.stack();
    msg.push_str(&stack);
    msg.push_str("\n\n");

    let _ = print_to_console(&msg, false);
}

static ENGINE_STATE: OnceLock<RwLock<EngineState>> = OnceLock::new();
#[inline]
async fn read_engine_state() -> RwLockReadGuard<'static, EngineState> {
    unsafe { ENGINE_STATE.get().unwrap_unchecked() }
        .read()
        .await
}
#[inline]
async fn write_engine_state() -> RwLockWriteGuard<'static, EngineState> {
    unsafe { ENGINE_STATE.get().unwrap_unchecked() }
        .write()
        .await
}

static STACK: OnceLock<RwLock<Stack>> = OnceLock::new();
#[inline]
async fn read_stack() -> RwLockReadGuard<'static, Stack> {
    unsafe { STACK.get().unwrap_unchecked() }.read().await
}
#[inline]
async fn write_stack() -> RwLockWriteGuard<'static, Stack> {
    unsafe { STACK.get().unwrap_unchecked() }.write().await
}

#[wasm_bindgen]
pub fn init_engine() -> String {
    std::panic::set_hook(Box::new(panic_hook));
    init_engine_internal().map_or_else(|err| format!("error: {err}"), |_| String::new())
}

fn init_engine_internal() -> Result<(), Report> {
    let mut stack = Stack::new();
    let mut engine_state = create_default_context();
    engine_state = add_shell_command_context(engine_state);
    engine_state = add_extra_command_context(engine_state);

    let mut working_set = StateWorkingSet::new(&engine_state);
    let decls: [Box<dyn Command>; 16] = [
        Box::new(Ls),
        Box::new(Open),
        Box::new(Save),
        Box::new(Mkdir),
        Box::new(Mv),
        Box::new(Pwd),
        Box::new(Cd),
        Box::new(Rm),
        Box::new(Fetch),
        Box::new(Source),
        Box::new(Job),
        Box::new(JobList),
        Box::new(JobKill),
        Box::new(Sys),
        Box::new(Random),
        Box::new(Print),
    ];
    for decl in decls {
        working_set.add_decl(decl);
    }
    engine_state.merge_delta(working_set.delta)?;

    let mut config = Config::default();
    config.use_ansi_coloring = true.into();
    config.show_banner = nu_protocol::BannerKind::Full;
    config.hooks.display_output = Some("table".into_value(Span::unknown()));
    config.table.show_empty = false;
    engine_state.config = Arc::new(config);

    engine_state.set_signals(Signals::new(Arc::new(InterruptBool)));

    // source our "nu rc"
    let rc_path = get_vfs().join("/.env.nu").ok();
    let rc = rc_path.and_then(|env| env.exists().ok().and_then(|ok| ok.then_some(env)));
    if let Some(env) = rc {
        web_sys::console::log_1(&format!("Loading rc file: {}", env.as_str()).into());
        eval_file(&engine_state, &mut stack, &env, Span::unknown())?;
    }

    ENGINE_STATE
        .set(RwLock::new(engine_state))
        .map_err(|_| miette::miette!("ENGINE_STATE was already set!?"))?;
    STACK
        .set(RwLock::new(stack))
        .map_err(|_| miette::miette!("STACK was already set!?"))?;

    // web_sys::console::log_1(&"Hello, World!".into());

    Ok(())
}

async fn run_command_internal(input: &str) -> Result<(), CommandError> {
    let mut engine_state = unsafe { ENGINE_STATE.get().unwrap_unchecked() }
        .upgradable_read()
        .await;
    let (mut working_set, signals, config) = {
        let mut write_engine_state = RwLockUpgradableReadGuard::upgrade(engine_state).await;
        apply_pending_deltas(&mut write_engine_state).map_err(|e| CommandError {
            error: Report::new(e),
            start_offset: 0,
        })?;
        write_engine_state.add_env_var(
            "PWD".to_string(),
            get_pwd_string().into_value(Span::unknown()),
        );
        engine_state = RwLockWriteGuard::downgrade_to_upgradable(write_engine_state);

        (
            StateWorkingSet::new(&engine_state),
            engine_state.signals().clone(),
            engine_state.config.clone(),
        )
    };
    let start_offset = working_set.next_span_start();
    let block = parse(&mut working_set, Some("entry"), input.as_bytes(), false);

    let cmd_err = |err: ShellError| CommandError {
        error: Report::new(err),
        start_offset,
    };

    if let Some(err) = working_set.parse_errors.into_iter().next() {
        return Err(CommandError {
            error: Report::new(err),
            start_offset,
        });
    }
    if let Some(err) = working_set.compile_errors.into_iter().next() {
        return Err(CommandError {
            error: Report::new(err),
            start_offset,
        });
    }
    let delta = working_set.delta;

    let result = {
        let mut write_engine_state = RwLockUpgradableReadGuard::upgrade(engine_state).await;
        let mut stack = write_stack().await;
        write_engine_state.merge_delta(delta).map_err(cmd_err)?;
        let res = eval_block::<nu_protocol::debugger::WithoutDebug>(
            &mut write_engine_state,
            &mut stack,
            &block,
            PipelineData::Empty,
        );
        // Apply any deltas queued during command execution (e.g., from source-file)
        apply_pending_deltas(&mut write_engine_state).map_err(cmd_err)?;
        engine_state = RwLockWriteGuard::downgrade_to_upgradable(write_engine_state);
        res
    };

    let pipeline_data = result.map_err(cmd_err)?.body;

    // this is annoying but we have to collect here so we can uncover errors
    // before passing the data off to Table, because otherwise Table
    // can't properly handle the errors and panics (something about ShellErrorBridge
    // having a non IO error in it somehow idk i dont care really)
    // TODO: see if there is a way to do this without collecting the pipeline
    let pipeline_data = match pipeline_data {
        PipelineData::Empty => return Ok(()),
        PipelineData::Value(Value::Error { error, .. }, _) => {
            return Err(cmd_err(*error));
        }
        PipelineData::ByteStream(s, m) => match (s.span(), s.type_(), s.reader()) {
            (span, ty, Some(r)) => {
                use std::io::Read;
                let v = r
                    .bytes()
                    .collect::<Result<Vec<u8>, _>>()
                    .map_err(|e| cmd_err(ShellError::Io(IoError::new(e, span, None))))?;
                (v.len() > 0)
                    .then(|| {
                        PipelineData::byte_stream(
                            ByteStream::read(Cursor::new(v), span, signals, ty),
                            m,
                        )
                    })
                    .unwrap_or(PipelineData::Empty)
            }
            (_, _, None) => PipelineData::Empty,
        },
        PipelineData::ListStream(s, m) => {
            let span = s.span();
            let v = s
                .into_iter()
                .map(|val| val.unwrap_error().map_err(cmd_err))
                .collect::<Result<Vec<Value>, _>>()?;
            PipelineData::list_stream(ListStream::new(v.into_iter(), span, signals), m)
        }
        x => x,
    };

    let res = {
        let mut write_engine_state = RwLockUpgradableReadGuard::upgrade(engine_state).await;
        let mut stack = write_stack().await;
        eval_hook(
            &mut write_engine_state,
            &mut stack,
            Some(pipeline_data),
            vec![],
            config.hooks.display_output.as_ref().unwrap(),
            "display_output",
        )
        .map_err(cmd_err)?
    };

    match res {
        PipelineData::Empty => {}
        PipelineData::Value(v, _) => {
            print_to_console(&v.to_expanded_string("\n", &config), true).map_err(cmd_err)?;
        }
        PipelineData::ByteStream(s, _) => {
            for line in s.lines().into_iter().flatten() {
                let out = line.map_err(cmd_err)?; // TODO: do we turn this into a Value ??? or is returning err fine
                print_to_console(&out, true).map_err(cmd_err)?;
            }
        }
        PipelineData::ListStream(s, _) => {
            for item in s.into_iter() {
                let out = item
                    .unwrap_error()
                    .map_err(cmd_err)?
                    .to_expanded_string("\n", &config);
                print_to_console(&out, true).map_err(cmd_err)?;
            }
        }
    }

    Ok(())
}

#[wasm_bindgen]
pub fn run_command(input: String) -> Promise {
    set_interrupt(false);

    future_to_promise(async move {
        run_command_internal(&input)
            .map(|res| {
                res.map_or_else(
                    |cmd_err| {
                        Some(format_error(
                            cmd_err.error,
                            input.to_owned(),
                            cmd_err.start_offset,
                        ))
                    },
                    |_| None,
                )
            })
            .map(|res| {
                Ok(res
                    .map(|s| JsValue::from_str(&s))
                    .unwrap_or_else(JsValue::null))
            })
            .await
    })
}

#[wasm_bindgen]
pub fn get_pwd_string() -> String {
    // web_sys::console::log_1(&"before pwd".into());
    let pwd = get_pwd();
    // web_sys::console::log_1(&"after pwd".into());
    if pwd.is_root() {
        return "/".to_string();
    }
    pwd.as_str().to_string()
}
