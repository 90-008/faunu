use crate::console_log;
use futures::FutureExt;
use js_sys::Promise;
use nu_parser::{flatten_block, parse};
use nu_protocol::engine::StateWorkingSet;
use wasm_bindgen::prelude::*;
use wasm_bindgen_futures::future_to_promise;

use super::*;

pub mod context;
pub mod files;
pub mod helpers;
pub mod suggestions;
pub mod types;
pub mod variables;

pub use context::determine_context;
pub use suggestions::generate_suggestions;
pub use types::{CompletionContext, Suggestion};

#[wasm_bindgen]
pub fn completion(input: String, js_cursor_pos: usize) -> Promise {
    future_to_promise(completion_impl(input, js_cursor_pos).map(|s| Ok(JsValue::from_str(&s))))
}

pub async fn completion_impl(input: String, js_cursor_pos: usize) -> String {
    let engine_guard = read_engine_state().await;
    let stack_guard = crate::read_stack().await;
    let root = get_pwd();

    // Map UTF-16 cursor position (from JS) to Byte index (for Rust)
    let byte_pos = input
        .char_indices()
        .map(|(i, _)| i)
        .nth(js_cursor_pos)
        .unwrap_or(input.len());

    let (working_set, shapes, global_offset) = {
        let mut working_set = StateWorkingSet::new(&engine_guard);
        let global_offset = working_set.next_span_start();
        let block = parse(&mut working_set, None, input.as_bytes(), false);
        let shapes = flatten_block(&working_set, &block);
        (working_set, shapes, global_offset)
    };

    // Initial state logging
    console_log!(
        "[completion] Input: {input:?}, JS cursor: {js_cursor_pos}, byte cursor: {byte_pos}"
    );
    console_log!(
        "[completion] Found {count} shapes, global_offset: {global_offset}",
        count = shapes.len()
    );
    for (idx, (span, shape)) in shapes.iter().enumerate() {
        let (local_start, local_end) = (
            span.start.saturating_sub(global_offset),
            span.end.saturating_sub(global_offset),
        );
        console_log!(
            "[completion] Shape {idx}: {shape:?} at [{start}, {end}] (local: [{local_start}, {local_end}])",
            start = span.start,
            end = span.end
        );
    }

    // Determine completion context
    let context = determine_context(
        &input,
        &shapes,
        &working_set,
        &engine_guard,
        byte_pos,
        global_offset,
    );

    // Generate suggestions based on context
    let mut suggestions = generate_suggestions(
        &input,
        context,
        &working_set,
        &engine_guard,
        &stack_guard,
        &root,
        byte_pos,
    );

    drop(working_set);
    drop(engine_guard);

    suggestions.sort();
    let suggestions = serde_json::to_string(&suggestions).unwrap_or_else(|_| "[]".to_string());
    console_log!("{suggestions}");
    suggestions
}
