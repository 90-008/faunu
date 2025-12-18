use futures::stream::AbortHandle;
use nu_protocol::{
    ShellError, Signal, Span,
    engine::{EngineState, StateDelta},
};
use rust_embed::RustEmbed;
use std::{
    collections::HashMap,
    sync::{
        Arc, Mutex, OnceLock, RwLock,
        atomic::{AtomicUsize, Ordering},
    },
    time::{Duration, SystemTime, UNIX_EPOCH},
};
use vfs::{EmbeddedFS, OverlayFS, VfsError, VfsPath, error::VfsErrorKind};
use wasm_bindgen::prelude::*;

use crate::memory_fs::MemoryFS;

static ROOT: OnceLock<Arc<VfsPath>> = OnceLock::new();

fn init_vfs() -> Arc<VfsPath> {
    let memory_fs = VfsPath::new(MemoryFS::new());
    let embedded_fs = VfsPath::new(EmbeddedFS::<EmbeddedFiles>::new());
    let overlaid_fs = VfsPath::new(OverlayFS::new(&[memory_fs, embedded_fs]));
    Arc::new(overlaid_fs)
}

pub fn get_vfs() -> Arc<VfsPath> {
    ROOT.get_or_init(init_vfs).clone()
}

#[derive(RustEmbed, Debug)]
#[folder = "embedded/"]
#[exclude = ".gitkeep"]
pub struct EmbeddedFiles;

static PWD: OnceLock<RwLock<Arc<VfsPath>>> = OnceLock::new();

pub fn get_pwd() -> Arc<VfsPath> {
    PWD.get_or_init(|| RwLock::new(get_vfs()))
        .read()
        .unwrap()
        .clone()
}

pub fn set_pwd(path: Arc<VfsPath>) {
    *PWD.get_or_init(|| RwLock::new(get_vfs())).write().unwrap() = path;
}

pub fn to_shell_err(span: Span) -> impl Fn(VfsError) -> ShellError {
    move |vfs_error: VfsError| ShellError::GenericError {
        error: (match vfs_error.kind() {
            VfsErrorKind::DirectoryExists
            | VfsErrorKind::FileExists
            | VfsErrorKind::FileNotFound
            | VfsErrorKind::InvalidPath => "path error",
            _ => "io error",
        })
        .to_string(),
        msg: vfs_error.to_string(),
        span: Some(span),
        help: None,
        inner: vec![],
    }
}

pub struct TaskInfo {
    pub id: usize,
    pub description: String,
    pub handle: AbortHandle,
}

pub struct CallbackWrapper(js_sys::Function);
unsafe impl Send for CallbackWrapper {}
unsafe impl Sync for CallbackWrapper {}

static NEXT_TASK_ID: AtomicUsize = AtomicUsize::new(1);
static TASK_REGISTRY: OnceLock<Mutex<HashMap<usize, TaskInfo>>> = OnceLock::new();
static TASK_CALLBACK: OnceLock<Mutex<Option<CallbackWrapper>>> = OnceLock::new();

pub fn get_registry() -> &'static Mutex<HashMap<usize, TaskInfo>> {
    TASK_REGISTRY.get_or_init(|| Mutex::new(HashMap::new()))
}

#[wasm_bindgen]
pub fn register_task_count_callback(f: js_sys::Function) {
    let _ = TASK_CALLBACK.get_or_init(|| Mutex::new(None));
    if let Ok(mut guard) = TASK_CALLBACK.get().unwrap().lock() {
        *guard = Some(CallbackWrapper(f));
    }
}

pub fn notify_task_count() {
    if let Some(mutex) = TASK_CALLBACK.get() {
        if let Ok(guard) = mutex.lock() {
            if let Some(cb) = guard.as_ref() {
                let count = if let Ok(reg) = get_registry().lock() {
                    reg.len()
                } else {
                    0
                };
                let this = JsValue::NULL;
                let arg = JsValue::from(count as u32);
                let _ = cb.0.call1(&this, &arg);
            }
        }
    }
}

pub fn register_task(description: String, handle: AbortHandle) -> usize {
    let id = NEXT_TASK_ID.fetch_add(1, Ordering::Relaxed);
    if let Ok(mut reg) = get_registry().lock() {
        reg.insert(
            id,
            TaskInfo {
                id,
                description,
                handle,
            },
        );
    }
    notify_task_count();
    id
}

pub fn remove_task(id: usize) {
    if let Ok(mut reg) = get_registry().lock() {
        reg.remove(&id);
    }
    notify_task_count();
}

pub fn get_all_tasks() -> Vec<(usize, String)> {
    if let Ok(reg) = get_registry().lock() {
        reg.values()
            .map(|t| (t.id, t.description.clone()))
            .collect()
    } else {
        vec![]
    }
}

pub fn kill_task_by_id(id: usize) -> bool {
    if let Ok(mut reg) = get_registry().lock() {
        if let Some(task) = reg.remove(&id) {
            task.handle.abort();
            drop(reg);
            notify_task_count();
            return true;
        }
    }
    false
}

static PENDING_DELTAS: OnceLock<Mutex<Vec<StateDelta>>> = OnceLock::new();

pub fn queue_delta(delta: StateDelta) {
    let _ = PENDING_DELTAS.get_or_init(|| Mutex::new(Vec::new()));
    if let Ok(mut guard) = PENDING_DELTAS.get().unwrap().lock() {
        guard.push(delta);
    }
}

pub fn apply_pending_deltas(engine_state: &mut EngineState) -> Result<(), ShellError> {
    if let Some(mutex) = PENDING_DELTAS.get() {
        if let Ok(mut guard) = mutex.lock() {
            for delta in guard.drain(..) {
                engine_state.merge_delta(delta)?;
            }
        }
    }
    Ok(())
}

pub static CONSOLE_CALLBACK: OnceLock<Mutex<Option<CallbackWrapper>>> = OnceLock::new();

#[wasm_bindgen]
pub fn register_console_callback(f: js_sys::Function) {
    let _ = CONSOLE_CALLBACK.get_or_init(|| Mutex::new(None));
    if let Ok(mut guard) = CONSOLE_CALLBACK.get().unwrap().lock() {
        *guard = Some(CallbackWrapper(f));
    }
}

pub fn print_to_console(msg: &str, is_cmd: bool) -> Result<(), ShellError> {
    // if is_interrupted() {
    //     return Err(ShellError::Interrupted {
    //         span: Span::unknown(),
    //     });
    // }
    if let Some(mutex) = CONSOLE_CALLBACK.get() {
        if let Ok(guard) = mutex.lock() {
            if let Some(cb) = guard.as_ref() {
                let this = JsValue::NULL;
                let arg = JsValue::from_str(msg);
                let arg2 = JsValue::from_bool(is_cmd);
                let _ = cb.0.call2(&this, &arg, &arg2);
            }
        }
    }
    Ok(())
}

pub fn current_time() -> Option<SystemTime> {
    UNIX_EPOCH.checked_add(Duration::from_millis(js_sys::Date::now() as u64))
}

use js_sys::Int32Array;
use std::cell::RefCell;

// We use thread_local storage because the Wasm worker is single-threaded.
// This holds the reference to the SharedArrayBuffer view passed from JS.
thread_local! {
    pub static INTERRUPT_BUFFER: RefCell<Option<Int32Array>> = RefCell::new(None);
}

/// Called from JS to pass the SharedArrayBuffer view
#[wasm_bindgen]
pub fn set_interrupt_buffer(buffer: Int32Array) {
    INTERRUPT_BUFFER.with(|b| {
        *b.borrow_mut() = Some(buffer);
    });
}

/// Call this function periodically in your long-running loops!
/// Returns `true` if an interrupt was requested.
pub fn check_interrupt() -> bool {
    INTERRUPT_BUFFER.with(|b| {
        if let Some(buffer) = b.borrow().as_ref() {
            // Check index 0. If it's 1, an interrupt occurred.
            // We use Atomics to ensure we see the value written by the main thread.
            match js_sys::Atomics::load(buffer, 0) {
                Ok(1) => true,
                _ => false,
            }
        } else {
            false
        }
    })
}

pub fn set_interrupt(value: bool) {
    INTERRUPT_BUFFER.with(|b| {
        if let Some(buffer) = b.borrow().as_ref() {
            let _ = js_sys::Atomics::store(buffer, 0, value as i32);
        }
    });
}

pub struct InterruptBool;

impl Signal for InterruptBool {
    #[inline]
    fn get(&self) -> bool {
        check_interrupt()
    }
    #[inline]
    fn set(&self, value: bool) {
        set_interrupt(value);
    }
}
