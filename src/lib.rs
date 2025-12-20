use async_lock::{RwLock, RwLockReadGuard, RwLockUpgradableReadGuard, RwLockWriteGuard};
use futures::TryFutureExt;
use js_sys::Promise;
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
    fmt::Write,
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
        Cd, Eval, Fetch, Glob, Job, JobKill, JobList, Ls, Mkdir, Mv, Open, Print, Pwd, Random, Rm,
        Save, SourceFile, Sys,
    },
    default_context::add_shell_command_context,
    globals::{InterruptBool, get_pwd, get_vfs, print_to_console, set_interrupt},
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
pub fn init_engine() -> Promise {
    std::panic::set_hook(Box::new(panic_hook));
    future_to_promise(
        init_engine_internal()
            .map_ok(|_| JsValue::null())
            .map_err(|s| JsValue::from_str(&s)),
    )
}

async fn init_engine_internal() -> Result<(), String> {
    let mut engine_state = create_default_context();
    engine_state = add_shell_command_context(engine_state);
    engine_state = add_extra_command_context(engine_state);

    let mut working_set = StateWorkingSet::new(&engine_state);
    let decls: [Box<dyn Command>; 18] = [
        Box::new(Ls),
        Box::new(Open),
        Box::new(Save),
        Box::new(Mkdir),
        Box::new(Mv),
        Box::new(Pwd),
        Box::new(Cd),
        Box::new(Rm),
        Box::new(Fetch),
        Box::new(SourceFile),
        Box::new(Eval),
        Box::new(Job),
        Box::new(JobList),
        Box::new(JobKill),
        Box::new(Sys),
        Box::new(Random),
        Box::new(Print),
        Box::new(Glob),
    ];
    for decl in decls {
        working_set.add_decl(decl);
    }
    engine_state
        .merge_delta(working_set.delta)
        .map_err(CommandError::from)?;

    let mut config = Config::default();
    config.use_ansi_coloring = true.into();
    config.show_banner = nu_protocol::BannerKind::Full;
    config.hooks.display_output = Some("table".into_value(Span::unknown()));
    config.table.show_empty = false;
    engine_state.config = Arc::new(config);

    engine_state.set_signals(Signals::new(Arc::new(InterruptBool)));

    ENGINE_STATE
        .set(RwLock::new(engine_state))
        .map_err(|_| "ENGINE_STATE was already set!?".to_string())?;
    STACK
        .set(RwLock::new(Stack::new()))
        .map_err(|_| "STACK was already set!?".to_string())?;

    let mut startup_script = String::new();

    // source our "nu rc"
    let rc_path = get_vfs().join("/.env.nu").ok();
    let rc = rc_path.and_then(|env| env.exists().ok().and_then(|ok| ok.then_some(env)));
    if let Some(env) = rc {
        writeln!(&mut startup_script, "eval file {path}", path = env.as_str()).unwrap();
    }

    // add some aliases for some commands
    let aliases = ["alias l = ls", "alias la = ls -a", "alias . = eval file"];
    for alias in aliases {
        writeln!(&mut startup_script, "{alias}").unwrap();
    }

    run_command_internal(&startup_script).await?;

    Ok(())
}

async fn run_command_internal(input: &str) -> Result<(), String> {
    let mut engine_state = unsafe { ENGINE_STATE.get().unwrap_unchecked() }
        .upgradable_read()
        .await;
    let (mut working_set, signals, config) = {
        let mut write_engine_state = RwLockUpgradableReadGuard::upgrade(engine_state).await;
        // apply_pending_deltas(&mut write_engine_state).map_err(|e| CommandError {
        //     error: Report::new(e),
        //     start_offset: 0,
        // })?;
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

    let cmd_err = |err: ShellError| CommandError::new(err, input).with_start_offset(start_offset);

    if let Some(err) = working_set.parse_errors.into_iter().next() {
        return Err(CommandError::new(err, input)
            .with_start_offset(start_offset)
            .into());
    }
    if let Some(err) = working_set.compile_errors.into_iter().next() {
        return Err(CommandError::new(err, input)
            .with_start_offset(start_offset)
            .into());
    }
    let delta = working_set.delta;

    let result = {
        let mut write_engine_state = RwLockUpgradableReadGuard::upgrade(engine_state).await;
        let mut stack = write_stack().await;
        write_engine_state.merge_delta(delta).map_err(cmd_err)?;
        engine_state = RwLockWriteGuard::downgrade_to_upgradable(write_engine_state);
        let res = eval_block::<nu_protocol::debugger::WithoutDebug>(
            &engine_state,
            &mut stack,
            &block,
            PipelineData::Empty,
        );
        // apply_pending_deltas(&mut write_engine_state).map_err(cmd_err)?;
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
            return Err(cmd_err(*error).into());
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
            print_to_console(&v.to_expanded_string("\n", &config), true);
        }
        PipelineData::ByteStream(s, _) => {
            for line in s.lines().into_iter().flatten() {
                let out = line.map_err(cmd_err)?; // TODO: do we turn this into a Value ??? or is returning err fine
                print_to_console(&out, true);
            }
        }
        PipelineData::ListStream(s, _) => {
            for item in s.into_iter() {
                let out = item
                    .unwrap_error()
                    .map_err(cmd_err)?
                    .to_expanded_string("\n", &config);
                print_to_console(&out, true);
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
            .map_ok(|_| JsValue::null())
            .map_err(|s| JsValue::from_str(&s))
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
