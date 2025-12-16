use futures::FutureExt;
use js_sys::Promise;
use wasm_bindgen_futures::future_to_promise;

use super::*;

#[wasm_bindgen]
pub fn highlight(input: String) -> Promise {
    future_to_promise(highlight_impl(input).map(|s| Ok(JsValue::from_str(&s))))
}

pub async fn highlight_impl(input: String) -> String {
    let (global_offset, shapes) = {
        let engine_guard = read_engine_state().await;
        let mut working_set = StateWorkingSet::new(&engine_guard);
        let offset = working_set.next_span_start();
        let block = parse(&mut working_set, None, input.as_bytes(), false);
        (offset, flatten_block(&working_set, &block))
    };

    let mut output = String::new();
    let mut last_end = 0;

    for (span, shape) in shapes {
        // Adjust global span to local byte indices
        let local_start = span.start.saturating_sub(global_offset);
        let local_end = span.end.saturating_sub(global_offset);

        if local_start >= input.len() {
            continue;
        }
        let safe_end = std::cmp::min(local_end, input.len());

        if local_start > last_end {
            output.push_str(&input[last_end..local_start]);
        }

        let start_index = std::cmp::max(local_start, last_end);
        if start_index >= safe_end {
            continue;
        }

        // Colors corresponding to standard Nushell/Ansi map
        let color = match shape {
            FlatShape::Pipe | FlatShape::Redirection => "\x1b[35m", // Magenta
            FlatShape::Bool | FlatShape::Int | FlatShape::Float => "\x1b[35;1m", // Light Magenta (Numbers/Bool)
            FlatShape::External(_) | FlatShape::InternalCall(_) | FlatShape::Keyword => {
                "\x1b[32;1m"
            } // Green Bold
            FlatShape::String | FlatShape::StringInterpolation | FlatShape::RawString => "\x1b[36m", // Cyan
            FlatShape::Variable(_) | FlatShape::Flag => "\x1b[34m", // Blue
            FlatShape::Garbage => "\x1b[31;1m\x1b[47m",             // Red on White
            FlatShape::Signature => "\x1b[33;1m",                   // Yellow Bold
            // Closures, blocks, punctuation often fall here
            FlatShape::Block => "\x1b[37m",     // White
            FlatShape::Closure => "\x1b[36;1m", // Light Cyan
            _ => "\x1b[0m",
        };

        output.push_str(color);
        output.push_str(&input[start_index..safe_end]);
        output.push_str("\x1b[0m");

        last_end = safe_end;
    }

    if last_end < input.len() {
        output.push_str(&input[last_end..]);
    }

    output
}
