use nu_parser::FlatShape;
use nu_protocol::engine::StateWorkingSet;
use nu_protocol::{ENV_VARIABLE_ID, IN_VARIABLE_ID, NU_VARIABLE_ID, Span};

/// Macro for console logging that automatically converts formatted strings to JsValue
#[macro_export]
macro_rules! console_log {
    ($($arg:tt)*) => {
        #[cfg(debug_assertions)]
        web_sys::console::log_1(&wasm_bindgen::JsValue::from_str(&format!($($arg)*)));
    };
}

pub fn is_separator_char(c: char) -> bool {
    ['|', ';', '(', '{'].contains(&c)
}

pub fn is_command_separator_char(c: char) -> bool {
    ['|', ';'].contains(&c)
}

pub fn has_separator_between(input: &str, start: usize, end: usize) -> bool {
    if start < end && start < input.len() {
        let text_between = &input[start..std::cmp::min(end, input.len())];
        text_between.chars().any(|c| is_separator_char(c))
    } else {
        false
    }
}

pub fn find_last_separator_pos(text: &str) -> Option<usize> {
    text.rfind(|c| is_command_separator_char(c)).map(|i| i + 1)
}

pub fn ends_with_separator(text: &str) -> bool {
    let text = text.trim_end();
    text.ends_with('|') || text.ends_with(';')
}

pub fn to_local_span(span: Span, global_offset: usize) -> Span {
    Span::new(
        span.start.saturating_sub(global_offset),
        span.end.saturating_sub(global_offset),
    )
}

pub fn safe_slice(input: &str, span: Span) -> &str {
    if span.start < input.len() {
        let safe_end = std::cmp::min(span.end, input.len());
        &input[span.start..safe_end]
    } else {
        ""
    }
}

pub fn is_command_shape(input: &str, shape: &FlatShape, local_span: Span) -> bool {
    matches!(
        shape,
        FlatShape::External(_) | FlatShape::InternalCall(_) | FlatShape::Keyword
    ) || matches!(shape, FlatShape::Garbage) && {
        if local_span.start < input.len() {
            let prev_text = safe_slice(input, local_span);
            !prev_text.trim().starts_with('-')
        } else {
            false
        }
    }
}

pub fn handle_block_prefix(prefix: &str, span: Span) -> Option<(&str, Span, bool)> {
    let mut block_prefix = prefix;
    let mut block_span_start = span.start;

    // Remove leading '{' and whitespace
    if block_prefix.starts_with('{') {
        block_prefix = &block_prefix[1..];
        block_span_start += 1;
    }
    let trimmed_block_prefix = block_prefix.trim_start();
    if trimmed_block_prefix != block_prefix {
        // Adjust span start to skip whitespace
        block_span_start += block_prefix.len() - trimmed_block_prefix.len();
    }

    let is_empty = trimmed_block_prefix.is_empty();
    Some((
        trimmed_block_prefix,
        Span::new(block_span_start, span.end),
        is_empty,
    ))
}

pub fn extract_command_name(cmd_text: &str) -> &str {
    cmd_text
        .split_whitespace()
        .next()
        .unwrap_or(cmd_text)
        .trim()
}

pub fn lookup_variable_id(
    var_name: &str,
    working_set: &StateWorkingSet,
) -> Option<nu_protocol::VarId> {
    match var_name {
        "env" => Some(ENV_VARIABLE_ID),
        "nu" => Some(NU_VARIABLE_ID),
        "in" => Some(IN_VARIABLE_ID),
        _ => working_set.find_variable(var_name.as_bytes()),
    }
}

pub fn parse_cell_path(text: &str) -> Option<(&str, Vec<&str>, &str)> {
    let trimmed = text.trim();
    if !trimmed.starts_with('$') {
        return None;
    }

    // Check if this is a cell path (contains a dot after $)
    if let Some(dot_pos) = trimmed[1..].find('.') {
        let var_name = &trimmed[1..dot_pos + 1];
        let after_var = &trimmed[dot_pos + 2..];
        let parts: Vec<&str> = after_var.split('.').collect();
        let (path_so_far, cell_prefix) = if parts.is_empty() {
            (vec![], "")
        } else if after_var.ends_with('.') {
            (
                parts.iter().filter(|s| !s.is_empty()).copied().collect(),
                "",
            )
        } else {
            let path: Vec<&str> = parts[..parts.len().saturating_sub(1)]
                .iter()
                .copied()
                .collect();
            let prefix = parts.last().copied().unwrap_or("");
            (path, prefix)
        };
        Some((var_name, path_so_far, cell_prefix))
    } else {
        None
    }
}

pub fn parse_cell_path_from_fields(text: &str) -> (Vec<&str>, &str) {
    let trimmed = text.trim();
    let parts: Vec<&str> = trimmed.split('.').collect();
    if parts.is_empty() {
        (vec![], "")
    } else if trimmed.ends_with('.') {
        (
            parts.iter().filter(|s| !s.is_empty()).copied().collect(),
            "",
        )
    } else {
        let path: Vec<&str> = parts[..parts.len().saturating_sub(1)]
            .iter()
            .copied()
            .collect();
        let prefix = parts.last().copied().unwrap_or("");
        (path, prefix)
    }
}

pub fn to_char_span(input: &str, span: Span) -> Span {
    let char_start = input[..span.start].chars().count();
    let char_end = input[..span.end].chars().count();
    Span::new(char_start, char_end)
}
