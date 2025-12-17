use futures::FutureExt;
use js_sys::Promise;
use wasm_bindgen_futures::future_to_promise;
use std::collections::HashMap;
use nu_protocol::{ENV_VARIABLE_ID, IN_VARIABLE_ID, NU_VARIABLE_ID, Value};

use super::*;

#[derive(Debug, Serialize)]
struct Suggestion {
    name: String,
    description: Option<String>,
    is_command: bool,
    rendered: String,
    span_start: usize, // char index (not byte)
    span_end: usize,   // char index (not byte)
}

impl PartialEq for Suggestion {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}
impl Eq for Suggestion {}
impl PartialOrd for Suggestion {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.name.partial_cmp(&other.name)
    }
}
impl Ord for Suggestion {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.name.cmp(&other.name)
    }
}

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
    web_sys::console::log_1(&JsValue::from_str(&format!(
        "[completion] Input: {:?}, JS cursor: {}, byte cursor: {}",
        input, js_cursor_pos, byte_pos
    )));
    web_sys::console::log_1(&JsValue::from_str(&format!(
        "[completion] Found {} shapes, global_offset: {}",
        shapes.len(),
        global_offset
    )));
    for (idx, (span, shape)) in shapes.iter().enumerate() {
        let (local_start, local_end) = (
            span.start.saturating_sub(global_offset),
            span.end.saturating_sub(global_offset),
        );
        web_sys::console::log_1(&JsValue::from_str(&format!(
            "[completion] Shape {}: {:?} at [{}, {}] (local: [{}, {}])",
            idx, shape, span.start, span.end, local_start, local_end
        )));
    }

    // Helper functions
    let is_separator_char = |c: char| -> bool { ['|', ';', '(', '{'].contains(&c) };

    let is_command_separator_char = |c: char| -> bool { ['|', ';'].contains(&c) };

    let has_separator_between = |start: usize, end: usize| -> bool {
        if start < end && start < input.len() {
            let text_between = &input[start..std::cmp::min(end, input.len())];
            text_between.chars().any(|c| is_separator_char(c))
        } else {
            false
        }
    };

    let find_last_separator_pos = |text: &str| -> Option<usize> {
        text.rfind(|c| is_command_separator_char(c)).map(|i| i + 1)
    };

    let ends_with_separator = |text: &str| -> bool {
        let text = text.trim_end();
        text.ends_with('|') || text.ends_with(';')
    };

    let to_local_span = |span: Span| -> Span {
        Span::new(
            span.start.saturating_sub(global_offset),
            span.end.saturating_sub(global_offset),
        )
    };

    let safe_slice = |span: Span| -> String {
        (span.start < input.len())
            .then(|| {
                let safe_end = std::cmp::min(span.end, input.len());
                input[span.start..safe_end].to_string()
            })
            .unwrap_or_default()
    };

    let is_command_shape = |shape: &FlatShape, local_span: Span| -> bool {
        matches!(
            shape,
            FlatShape::External(_) | FlatShape::InternalCall(_) | FlatShape::Keyword
        ) || matches!(shape, FlatShape::Garbage) && {
            if local_span.start < input.len() {
                let prev_text = &safe_slice(local_span);
                !prev_text.trim().starts_with('-')
            } else {
                false
            }
        }
    };

    let handle_block_prefix = |prefix: &str, span: Span| -> Option<(String, Span, bool)> {
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
            trimmed_block_prefix.to_string(),
            Span::new(block_span_start, span.end),
            is_empty,
        ))
    };

    // Helper function to find command name and count arguments before cursor
    let find_command_and_arg_index =
        |current_idx: usize, current_local_span: Span| -> Option<(String, usize)> {
            let mut command_name: Option<String> = None;
            let mut arg_count = 0;

            // Look backwards through shapes to find the command
            for i in (0..current_idx).rev() {
                if let Some((prev_span, prev_shape)) = shapes.get(i) {
                    let prev_local_span = to_local_span(*prev_span);

                    // Check if there's a separator between this shape and the next one
                    let next_shape_start = if i + 1 < shapes.len() {
                        to_local_span(shapes[i + 1].0).start
                    } else {
                        current_local_span.start
                    };

                    if has_separator_between(prev_local_span.end, next_shape_start) {
                        break; // Stop at separator
                    }

                    if is_command_shape(prev_shape, prev_local_span) {
                        // Found the command
                        let cmd_text = safe_slice(prev_local_span);
                        // Extract just the command name (first word, no flags)
                        let cmd_name = cmd_text
                            .split_whitespace()
                            .next()
                            .unwrap_or(&cmd_text)
                            .trim();
                        command_name = Some(cmd_name.to_string());
                        break;
                    } else {
                        // This is an argument - count it if it's not a flag
                        let arg_text = safe_slice(prev_local_span);
                        let trimmed_arg = arg_text.trim();
                        // Don't count flags (starting with -) or empty arguments
                        if !trimmed_arg.is_empty() && !trimmed_arg.starts_with('-') {
                            arg_count += 1;
                        }
                    }
                }
            }

            command_name.map(|name| (name, arg_count))
        };

    // Helper function to handle both Block and Closure shapes
    let handle_block_or_closure = |prefix: &str,
                                   span: Span,
                                   shape_name: &str,
                                   current_idx: usize,
                                   local_span: Span|
     -> Option<CompletionContext> {
        web_sys::console::log_1(&JsValue::from_str(&format!(
            "[completion] Processing {} shape with prefix: {:?}",
            shape_name, prefix
        )));

        // Check if the content ends with a pipe or semicolon
        let prefix_ends_with_separator = ends_with_separator(prefix);
        let last_sep_pos_in_prefix = if prefix_ends_with_separator {
            find_last_separator_pos(prefix)
        } else {
            None
        };
        web_sys::console::log_1(&JsValue::from_str(&format!(
            "[completion] {}: prefix_ends_with_separator={}, last_sep_pos_in_prefix={:?}",
            shape_name, prefix_ends_with_separator, last_sep_pos_in_prefix
        )));

        if let Some((trimmed_prefix, adjusted_span, is_empty)) = handle_block_prefix(prefix, span) {
            web_sys::console::log_1(&JsValue::from_str(&format!(
                "[completion] {}: trimmed_prefix={:?}, is_empty={}",
                shape_name, trimmed_prefix, is_empty
            )));

            if is_empty {
                // Empty block/closure or just whitespace - command context
                web_sys::console::log_1(&JsValue::from_str(&format!(
                    "[completion] {} is empty, setting Command context",
                    shape_name
                )));
                Some(CompletionContext::Command {
                    prefix: String::new(),
                    span: adjusted_span,
                })
            } else if let Some(last_sep_pos) = last_sep_pos_in_prefix {
                // After a separator - command context
                let after_sep = prefix[last_sep_pos..].trim_start();
                web_sys::console::log_1(&JsValue::from_str(&format!(
                    "[completion] {} has separator at {}, after_sep={:?}, setting Command context",
                    shape_name, last_sep_pos, after_sep
                )));
                Some(CompletionContext::Command {
                    prefix: after_sep.to_string(),
                    span: Span::new(span.start + last_sep_pos, span.end),
                })
            } else {
                web_sys::console::log_1(&JsValue::from_str(&format!(
                    "[completion] {} has no separator, checking for variable/flag/argument context",
                    shape_name
                )));
                // Check if this is a variable or cell path first
                let trimmed = trimmed_prefix.trim();
                
                if trimmed.starts_with('$') {
                    // Variable or cell path completion
                    if let Some(dot_pos) = trimmed[1..].find('.') {
                        // Cell path completion: $in.name, $env.PWD, etc.
                        let var_name = &trimmed[1..dot_pos + 1];
                        let after_var = &trimmed[dot_pos + 2..];
                        let parts: Vec<&str> = after_var.split('.').collect();
                        let (path_so_far, cell_prefix) = if parts.is_empty() {
                            (vec![], String::new())
                        } else if after_var.ends_with('.') {
                            (parts.iter().filter(|s| !s.is_empty()).map(|s| s.to_string()).collect(), String::new())
                        } else {
                            let path: Vec<String> = parts[..parts.len().saturating_sub(1)].iter().map(|s| s.to_string()).collect();
                            let prefix = parts.last().map(|s| s.to_string()).unwrap_or_default();
                            (path, prefix)
                        };
                        
                        let var_id = match var_name {
                            "env" => Some(ENV_VARIABLE_ID),
                            "nu" => Some(NU_VARIABLE_ID),
                            "in" => Some(IN_VARIABLE_ID),
                            _ => working_set.find_variable(var_name.as_bytes())
                        };
                        
                        if let Some(var_id) = var_id {
                            let prefix_byte_len = cell_prefix.len();
                            let cell_span_start = adjusted_span.end.saturating_sub(prefix_byte_len);
                            web_sys::console::log_1(&JsValue::from_str(&format!(
                                "[completion] {}: Setting CellPath context with var {:?}, prefix {:?}",
                                shape_name, var_name, cell_prefix
                            )));
                            Some(CompletionContext::CellPath {
                                prefix: cell_prefix,
                                span: Span::new(cell_span_start, adjusted_span.end),
                                var_id,
                                path_so_far,
                            })
                        } else {
                            // Unknown variable, fall back to variable completion
                            let var_prefix = trimmed[1..].to_string();
                            web_sys::console::log_1(&JsValue::from_str(&format!(
                                "[completion] {}: Unknown var, setting Variable context with prefix {:?}",
                                shape_name, var_prefix
                            )));
                            Some(CompletionContext::Variable {
                                prefix: var_prefix,
                                span: adjusted_span,
                            })
                        }
                    } else {
                        // Simple variable completion (no dot)
                        let var_prefix = if trimmed.len() > 1 {
                            trimmed[1..].to_string()
                        } else {
                            String::new()
                        };
                        web_sys::console::log_1(&JsValue::from_str(&format!(
                            "[completion] {}: Setting Variable context with prefix {:?}",
                            shape_name, var_prefix
                        )));
                        Some(CompletionContext::Variable {
                            prefix: var_prefix,
                            span: adjusted_span,
                        })
                    }
                } else if trimmed.starts_with('-') {
                    // Flag completion
                    if let Some((cmd_name, _)) = find_command_and_arg_index(current_idx, local_span) {
                        web_sys::console::log_1(&JsValue::from_str(&format!(
                            "[completion] {}: Found command {:?} for flag completion",
                            shape_name, cmd_name
                        )));
                        Some(CompletionContext::Flag {
                            prefix: trimmed.to_string(),
                            span: adjusted_span,
                            command_name: cmd_name,
                        })
                    } else {
                        Some(CompletionContext::Argument {
                            prefix: trimmed_prefix,
                            span: adjusted_span,
                        })
                    }
                } else {
                    // Try to find the command and argument index
                    if let Some((cmd_name, arg_index)) =
                        find_command_and_arg_index(current_idx, local_span)
                    {
                        web_sys::console::log_1(&JsValue::from_str(&format!(
                            "[completion] {}: Found command {:?} with arg_index {} for argument completion",
                            shape_name, cmd_name, arg_index
                        )));
                        Some(CompletionContext::CommandArgument {
                            prefix: trimmed.to_string(),
                            span: adjusted_span,
                            command_name: cmd_name,
                            arg_index,
                        })
                    } else {
                        // No command found, treat as regular argument
                        web_sys::console::log_1(&JsValue::from_str(&format!(
                            "[completion] {}: No command found, using Argument context",
                            shape_name
                        )));
                        Some(CompletionContext::Argument {
                            prefix: trimmed_prefix,
                            span: adjusted_span,
                        })
                    }
                }
            }
        } else {
            None
        }
    };

    // Helper function to evaluate a variable for completion
    // Returns the Value of a variable if it can be evaluated
    let eval_variable_for_completion = |var_id: nu_protocol::VarId, working_set: &StateWorkingSet| -> Option<Value> {
        match var_id {
            id if id == NU_VARIABLE_ID => {
                // $nu - get from engine state constant
                engine_guard.get_constant(id).cloned()
            }
            id if id == ENV_VARIABLE_ID => {
                // $env - build from environment variables in engine state
                // EnvVars is HashMap<String, HashMap<String, Value>> (overlay -> vars)
                let mut pairs: Vec<(String, Value)> = Vec::new();
                for overlay_env in engine_guard.env_vars.values() {
                    for (name, value) in overlay_env.iter() {
                        pairs.push((name.clone(), value.clone()));
                    }
                }
                pairs.sort_by(|a, b| a.0.cmp(&b.0));
                // Deduplicate by name (later overlays override earlier ones)
                pairs.dedup_by(|a, b| a.0 == b.0);
                Some(Value::record(pairs.into_iter().collect(), Span::unknown()))
            }
            id if id == IN_VARIABLE_ID => {
                // $in - typically not available at completion time
                None
            }
            _ => {
                // User-defined variable - try to get const value first
                let var_info = working_set.get_variable(var_id);
                if let Some(const_val) = &var_info.const_val {
                    Some(const_val.clone())
                } else {
                    // Variable doesn't have a const value (runtime value)
                    // Try to get the value from the stack (runtime storage)
                    match stack_guard.get_var(var_id, Span::unknown()) {
                        Ok(value) => {
                            web_sys::console::log_1(&JsValue::from_str(&format!(
                                "[completion] Found variable {:?} value in stack",
                                var_id
                            )));
                            Some(value)
                        }
                        Err(_) => {
                            // Variable not in stack either
                            web_sys::console::log_1(&JsValue::from_str(&format!(
                                "[completion] Variable {:?} has no const value and not in stack, type: {:?}",
                                var_id, var_info.ty
                            )));
                            None
                        }
                    }
                }
            }
        }
    };
    
    // Helper function to extract column/field names from a Value
    let get_columns_from_value = |value: &Value| -> Vec<(String, Option<String>)> {
        match value {
            Value::Record { val, .. } => {
                val.iter()
                    .map(|(name, v)| (name.to_string(), Some(v.get_type().to_string())))
                    .collect()
            }
            Value::List { vals, .. } => {
                // Get common columns from list of records
                if let Some(first) = vals.first() {
                    if let Value::Record { val, .. } = first {
                        return val.iter()
                            .map(|(name, v)| (name.to_string(), Some(v.get_type().to_string())))
                            .collect();
                    }
                }
                vec![]
            }
            _ => vec![],
        }
    };
    
    // Helper function to follow a cell path and get the value at that path
    let follow_cell_path = |value: &Value, path: &[String]| -> Option<Value> {
        let mut current = value.clone();
        for member in path {
            match &current {
                Value::Record { val, .. } => {
                    current = val.get(member)?.clone();
                }
                Value::List { vals, .. } => {
                    // Try to parse as index or get from first record
                    if let Ok(idx) = member.parse::<usize>() {
                        current = vals.get(idx)?.clone();
                    } else if let Some(first) = vals.first() {
                        if let Value::Record { val, .. } = first {
                            current = val.get(member)?.clone();
                        } else {
                            return None;
                        }
                    } else {
                        return None;
                    }
                }
                _ => return None,
            }
        }
        Some(current)
    };

    // Helper function to extract closure parameters from input string at cursor position
    // We parse the input directly to find closures containing the cursor and extract their parameters
    let extract_closure_params = |input: &str, cursor_pos: usize| -> Vec<String> {
        let mut params = Vec::new();
        
        // Find all closures in the input by looking for {|...| patterns
        // We need to find closures that contain the cursor position
        let mut brace_stack: Vec<usize> = Vec::new(); // Stack of opening brace positions
        let mut closures: Vec<(usize, usize, Vec<String>)> = Vec::new(); // (start, end, params)
        
        let mut i = 0;
        let chars: Vec<char> = input.chars().collect();
        
        while i < chars.len() {
            if chars[i] == '{' {
                brace_stack.push(i);
            } else if chars[i] == '}' {
                if let Some(start) = brace_stack.pop() {
                    // Check if this is a closure with parameters: {|param| ...}
                    if start + 1 < chars.len() && chars[start + 1] == '|' {
                        // Find the parameter list
                        let param_start = start + 2;
                        let mut param_end = param_start;
                        
                        // Find the closing | of the parameter list
                        while param_end < chars.len() && chars[param_end] != '|' {
                            param_end += 1;
                        }
                        
                        if param_end < chars.len() {
                            // Extract parameter names
                            let params_text: String = chars[param_start..param_end].iter().collect();
                            let param_names: Vec<String> = params_text
                                .split(',')
                                .map(|s| s.trim().to_string())
                                .filter(|s| !s.is_empty())
                                .collect();
                            
                            closures.push((start, i + 1, param_names));
                        }
                    }
                }
            }
            i += 1;
        }
        
        // Find closures that contain the cursor position
        // A closure contains the cursor if: start <= cursor_pos < end
        for (start, end, param_names) in closures {
            if start <= cursor_pos && cursor_pos < end {
                web_sys::console::log_1(&JsValue::from_str(&format!(
                    "[completion] Found closure at [{}, {}) containing cursor {}, params: {:?}",
                    start, end, cursor_pos, param_names
                )));
                params.extend(param_names);
            }
        }
        
        params
    };

    // Helper function to collect variables from working set
    let collect_variables = |working_set: &StateWorkingSet, input: &str, cursor_pos: usize| -> HashMap<String, nu_protocol::VarId> {
        let mut variables = HashMap::new();
        
        // Add built-in variables
        variables.insert("$nu".to_string(), NU_VARIABLE_ID);
        variables.insert("$in".to_string(), IN_VARIABLE_ID);
        variables.insert("$env".to_string(), ENV_VARIABLE_ID);
        
        // Collect closure parameters at cursor position
        // We don't need real var_ids for closure parameters since they're not evaluated yet
        // We'll use a placeholder var_id (using IN_VARIABLE_ID as a safe placeholder)
        // The actual var_id lookup will happen when the variable is used
        let closure_params = extract_closure_params(input, cursor_pos);
        for param_name in closure_params {
            let var_name = format!("${}", param_name);
            // Use IN_VARIABLE_ID as placeholder - it's safe since we're just using it for the name
            // The completion logic only needs the name, not the actual var_id
            variables.insert(var_name.clone(), IN_VARIABLE_ID);
            web_sys::console::log_1(&JsValue::from_str(&format!(
                "[completion] Added closure parameter: {:?}",
                var_name
            )));
        }
        
        // Collect from working set delta scope
        let mut removed_overlays = vec![];
        for scope_frame in working_set.delta.scope.iter().rev() {
            for overlay_frame in scope_frame.active_overlays(&mut removed_overlays).rev() {
                for (name, var_id) in &overlay_frame.vars {
                    let name = String::from_utf8_lossy(name).to_string();
                    variables.insert(name, *var_id);
                }
            }
        }
        
        // Collect from permanent state scope
        for overlay_frame in working_set
            .permanent_state
            .active_overlays(&removed_overlays)
            .rev()
        {
            for (name, var_id) in &overlay_frame.vars {
                let name = String::from_utf8_lossy(name).to_string();
                variables.insert(name, *var_id);
            }
        }
        
        variables
    };

    // Find what we're completing
    #[derive(Debug)]
    enum CompletionContext {
        Command {
            prefix: String,
            span: Span,
        },
        Argument {
            prefix: String,
            span: Span,
        },
        Flag {
            prefix: String,
            span: Span,
            command_name: String,
        },
        CommandArgument {
            prefix: String,
            span: Span,
            command_name: String,
            arg_index: usize,
        },
        Variable {
            prefix: String, // without the $ prefix
            span: Span,
        },
        CellPath {
            prefix: String,       // the partial field name being typed (after the last dot)
            span: Span,           // replacement span
            var_id: nu_protocol::VarId, // variable ID for evaluation
            path_so_far: Vec<String>, // path members accessed before current one
        },
    }

    let mut context: Option<CompletionContext> = None;

    // Helper function to build full command prefix by looking backwards through shapes
    let build_command_prefix =
        |current_idx: usize, current_local_span: Span, current_prefix: &str| -> (String, Span) {
            let mut span_start = current_local_span.start;

            // Look backwards through shapes to find previous command words
            for i in (0..current_idx).rev() {
                if let Some((prev_span, prev_shape)) = shapes.get(i) {
                    let prev_local_span = to_local_span(*prev_span);

                    if is_command_shape(prev_shape, prev_local_span) {
                        // Check if there's a separator between this shape and the next one
                        let next_shape_start = if i + 1 < shapes.len() {
                            to_local_span(shapes[i + 1].0).start
                        } else {
                            current_local_span.start
                        };

                        // Check if there's a separator (pipe, semicolon, etc.) between shapes
                        // Whitespace is fine, but separators indicate a new command
                        if has_separator_between(prev_local_span.end, next_shape_start) {
                            break; // Stop at separator
                        }

                        // Update span start to include this command word
                        span_start = prev_local_span.start;
                    } else {
                        // Not a command shape, stop looking backwards
                        break;
                    }
                }
            }

            // Extract the full prefix from the input, preserving exact spacing
            let span_end = current_local_span.end;
            let full_prefix = if span_start < input.len() {
                safe_slice(Span::new(span_start, span_end))
            } else {
                current_prefix.to_string()
            };

            (full_prefix, Span::new(span_start, span_end))
        };

    // First, check if cursor is within a shape
    for (idx, (span, shape)) in shapes.iter().enumerate() {
        let local_span = to_local_span(*span);

        if local_span.start <= byte_pos && byte_pos <= local_span.end {
            web_sys::console::log_1(&JsValue::from_str(&format!(
                "[completion] Cursor in shape {}: {:?} at {:?}",
                idx, shape, local_span
            )));

            // Check if there's a pipe or semicolon between this shape's end and the cursor
            // If so, we're starting a new command and should ignore this shape
            let has_sep = has_separator_between(local_span.end, byte_pos);
            if has_sep {
                web_sys::console::log_1(&JsValue::from_str(&format!(
                    "[completion] Separator found between shape end ({}) and cursor ({}), skipping shape",
                    local_span.end, byte_pos
                )));
                // There's a separator, so we're starting a new command - skip this shape
                continue;
            }

            let span = Span::new(local_span.start, std::cmp::min(local_span.end, byte_pos));
            let prefix = safe_slice(span);
            web_sys::console::log_1(&JsValue::from_str(&format!(
                "[completion] Processing shape {} with prefix: {:?}",
                idx, prefix
            )));

            // Special case: if prefix is just '{' (possibly with whitespace),
            // we're at the start of a block and should complete commands
            let trimmed_prefix = prefix.trim();
            if trimmed_prefix == "{" {
                // We're right after '{' - command context
                if let Some((_, adjusted_span, _)) = handle_block_prefix(&prefix, span) {
                    context = Some(CompletionContext::Command {
                        prefix: String::new(),
                        span: adjusted_span,
                    });
                }
            } else {
                match shape {
                    // Special case: Check if we're completing a cell path where the Variable and field are in separate shapes
                    // e.g., `$a.na` where $a is a Variable shape and `.na` is a String shape
                    _ if {
                        idx > 0 && matches!(shape, FlatShape::String)
                    } => {
                        // Look at the previous shape to see if it's a Variable
                        let prev_shape = &shapes[idx - 1];
                        let prev_local_span = to_local_span(prev_shape.0);
                        
                        if let FlatShape::Variable(var_id) = prev_shape.1 {
                            // Check if the variable shape ends right where this shape starts (or very close)
                            // Allow for a small gap (like a dot) between shapes
                            let gap = local_span.start.saturating_sub(prev_local_span.end);
                            if gap <= 1 {
                                // This is a cell path - the String shape contains the field name(s)
                                // The prefix might be like "na" or "field.subfield"
                                let trimmed_prefix = prefix.trim();
                                let parts: Vec<&str> = trimmed_prefix.split('.').collect();
                                let (path_so_far, cell_prefix) = if parts.is_empty() {
                                    (vec![], String::new())
                                } else if trimmed_prefix.ends_with('.') {
                                    (parts.iter().filter(|s| !s.is_empty()).map(|s| s.to_string()).collect(), String::new())
                                } else {
                                    let path: Vec<String> = parts[..parts.len().saturating_sub(1)].iter().map(|s| s.to_string()).collect();
                                    let prefix = parts.last().map(|s| s.to_string()).unwrap_or_default();
                                    (path, prefix)
                                };
                                
                                let prefix_byte_len = cell_prefix.len();
                                let cell_span_start = span.end.saturating_sub(prefix_byte_len);
                                web_sys::console::log_1(&JsValue::from_str(&format!(
                                    "[completion] Detected cell path from Variable+String shapes, var_id={:?}, prefix={:?}, path={:?}",
                                    var_id, cell_prefix, path_so_far
                                )));
                                context = Some(CompletionContext::CellPath {
                                    prefix: cell_prefix,
                                    span: Span::new(cell_span_start, span.end),
                                    var_id,
                                    path_so_far,
                                });
                            } else {
                                // Gap between shapes, fall through to default handling
                                context = Some(CompletionContext::Argument { prefix, span });
                            }
                        } else {
                            // Previous shape is not a Variable, this is likely a regular string
                            context = Some(CompletionContext::Argument { prefix, span });
                        }
                    }
                    // Special case: Check if we're completing a cell path where the Variable and dot are in separate shapes
                    // e.g., `{ $in. }` where $in is Shape 4 (Variable) and `. }` is Shape 5 (Block)
                    _ if {
                        let trimmed_prefix = prefix.trim();
                        trimmed_prefix.starts_with('.') && idx > 0
                    } => {
                        // Look at the previous shape to see if it's a Variable
                        let prev_shape = &shapes[idx - 1];
                        let prev_local_span = to_local_span(prev_shape.0);
                        
                        if let FlatShape::Variable(var_id) = prev_shape.1 {
                            // Check if the variable shape ends right where this shape starts
                            if prev_local_span.end == local_span.start {
                                let trimmed_prefix = prefix.trim();
                                // Parse path members from the prefix (which is like ".field" or ".field.subfield")
                                let after_dot = &trimmed_prefix[1..]; // Remove leading dot
                                let parts: Vec<&str> = after_dot.split('.').collect();
                                let (path_so_far, cell_prefix) = if parts.is_empty() || (parts.len() == 1 && parts[0].is_empty()) {
                                    (vec![], String::new())
                                } else if after_dot.ends_with('.') {
                                    (parts.iter().filter(|s| !s.is_empty()).map(|s| s.to_string()).collect(), String::new())
                                } else {
                                    let path: Vec<String> = parts[..parts.len().saturating_sub(1)].iter().map(|s| s.to_string()).collect();
                                    let prefix = parts.last().map(|s| s.to_string()).unwrap_or_default();
                                    (path, prefix)
                                };
                                
                                let prefix_byte_len = cell_prefix.len();
                                let cell_span_start = span.end.saturating_sub(prefix_byte_len);
                                web_sys::console::log_1(&JsValue::from_str(&format!(
                                    "[completion] Detected cell path from adjacent Variable shape, var_id={:?}, prefix={:?}",
                                    var_id, cell_prefix
                                )));
                                context = Some(CompletionContext::CellPath {
                                    prefix: cell_prefix,
                                    span: Span::new(cell_span_start, span.end),
                                    var_id,
                                    path_so_far,
                                });
                            } else {
                                // Gap between shapes, fall through to default handling
                                context = Some(CompletionContext::Argument { prefix, span });
                            }
                        } else {
                            // Previous shape is not a Variable, this is likely a file path starting with .
                            context = Some(CompletionContext::Argument { prefix, span });
                        }
                    }
                    _ if {
                        // Check if this is a variable or cell path (starts with $) before treating as command
                        let trimmed_prefix = prefix.trim();
                        trimmed_prefix.starts_with('$')
                    } => {
                        let trimmed_prefix = prefix.trim();
                        // Check if this is a cell path (contains a dot after $)
                        if let Some(dot_pos) = trimmed_prefix[1..].find('.') {
                            // Cell path completion: $env.PWD, $nu.home-path, etc.
                            let var_name = &trimmed_prefix[1..dot_pos + 1]; // e.g., "env"
                            let after_var = &trimmed_prefix[dot_pos + 2..]; // e.g., "PWD" or "config.color"
                            
                            // Parse path members and current prefix
                            let parts: Vec<&str> = after_var.split('.').collect();
                            let (path_so_far, cell_prefix) = if parts.is_empty() {
                                (vec![], String::new())
                            } else if after_var.ends_with('.') {
                                // Cursor is right after a dot, complete all fields
                                (parts.iter().filter(|s| !s.is_empty()).map(|s| s.to_string()).collect(), String::new())
                            } else {
                                // Cursor is in the middle of typing a field name
                                let path: Vec<String> = parts[..parts.len().saturating_sub(1)].iter().map(|s| s.to_string()).collect();
                                let prefix = parts.last().map(|s| s.to_string()).unwrap_or_default();
                                (path, prefix)
                            };
                            
                            // Find the variable ID
                            let var_id = match var_name {
                                "env" => Some(ENV_VARIABLE_ID),
                                "nu" => Some(NU_VARIABLE_ID),
                                "in" => Some(IN_VARIABLE_ID),
                                _ => {
                                    // Try to find user-defined variable
                                    working_set.find_variable(var_name.as_bytes())
                                }
                            };
                            
                            if let Some(var_id) = var_id {
                                // Calculate span for the cell path member being completed
                                let prefix_byte_len = cell_prefix.len();
                                let cell_span_start = span.end.saturating_sub(prefix_byte_len);
                                context = Some(CompletionContext::CellPath {
                                    prefix: cell_prefix,
                                    span: Span::new(cell_span_start, span.end),
                                    var_id,
                                    path_so_far,
                                });
                            } else {
                                // Unknown variable, fall back to variable completion
                                let var_prefix = trimmed_prefix[1..].to_string();
                                context = Some(CompletionContext::Variable {
                                    prefix: var_prefix,
                                    span,
                                });
                            }
                        } else {
                            // Variable completion context (no dot)
                            let var_prefix = if trimmed_prefix.len() > 1 {
                                trimmed_prefix[1..].to_string()
                            } else {
                                String::new()
                            };
                            context = Some(CompletionContext::Variable {
                                prefix: var_prefix,
                                span,
                            });
                        }
                    }
                    _ if is_command_shape(shape, local_span) => {
                        let (full_prefix, full_span) = build_command_prefix(idx, span, &prefix);
                        context = Some(CompletionContext::Command {
                            prefix: full_prefix,
                            span: full_span,
                        });
                    }
                    FlatShape::Block | FlatShape::Closure => {
                        if let Some(ctx) = handle_block_or_closure(
                            &prefix,
                            span,
                            shape.as_str().trim_start_matches("shape_"),
                            idx,
                            local_span,
                        ) {
                            context = Some(ctx);
                        }
                    }
                    FlatShape::Variable(var_id) => {
                        // Variable or cell path completion context
                        let trimmed_prefix = prefix.trim();
                        if trimmed_prefix.starts_with('$') {
                            // Check if this is a cell path (contains a dot after $)
                            if let Some(dot_pos) = trimmed_prefix[1..].find('.') {
                                // Cell path completion
                                let after_var = &trimmed_prefix[dot_pos + 2..];
                                let parts: Vec<&str> = after_var.split('.').collect();
                                let (path_so_far, cell_prefix) = if parts.is_empty() {
                                    (vec![], String::new())
                                } else if after_var.ends_with('.') {
                                    (parts.iter().filter(|s| !s.is_empty()).map(|s| s.to_string()).collect(), String::new())
                                } else {
                                    let path: Vec<String> = parts[..parts.len().saturating_sub(1)].iter().map(|s| s.to_string()).collect();
                                    let prefix = parts.last().map(|s| s.to_string()).unwrap_or_default();
                                    (path, prefix)
                                };
                                
                                let prefix_byte_len = cell_prefix.len();
                                let cell_span_start = span.end.saturating_sub(prefix_byte_len);
                                context = Some(CompletionContext::CellPath {
                                    prefix: cell_prefix,
                                    span: Span::new(cell_span_start, span.end),
                                    var_id: *var_id,
                                    path_so_far,
                                });
                            } else {
                                // Simple variable completion
                                let var_prefix = trimmed_prefix[1..].to_string();
                                context = Some(CompletionContext::Variable {
                                    prefix: var_prefix,
                                    span,
                                });
                            }
                        } else {
                            // Fallback to argument context if no $ found
                            context = Some(CompletionContext::Argument { prefix, span });
                        }
                    }
                    _ => {
                        // Check if this is a variable or cell path (starts with $)
                        let trimmed_prefix = prefix.trim();
                        if trimmed_prefix.starts_with('$') {
                            // Check if this is a cell path (contains a dot after $)
                            if let Some(dot_pos) = trimmed_prefix[1..].find('.') {
                                // Cell path completion
                                let var_name = &trimmed_prefix[1..dot_pos + 1];
                                let after_var = &trimmed_prefix[dot_pos + 2..];
                                let parts: Vec<&str> = after_var.split('.').collect();
                                let (path_so_far, cell_prefix) = if parts.is_empty() {
                                    (vec![], String::new())
                                } else if after_var.ends_with('.') {
                                    (parts.iter().filter(|s| !s.is_empty()).map(|s| s.to_string()).collect(), String::new())
                                } else {
                                    let path: Vec<String> = parts[..parts.len().saturating_sub(1)].iter().map(|s| s.to_string()).collect();
                                    let prefix = parts.last().map(|s| s.to_string()).unwrap_or_default();
                                    (path, prefix)
                                };
                                
                                let var_id = match var_name {
                                    "env" => Some(ENV_VARIABLE_ID),
                                    "nu" => Some(NU_VARIABLE_ID),
                                    "in" => Some(IN_VARIABLE_ID),
                                    _ => working_set.find_variable(var_name.as_bytes())
                                };
                                
                                if let Some(var_id) = var_id {
                                    let prefix_byte_len = cell_prefix.len();
                                    let cell_span_start = span.end.saturating_sub(prefix_byte_len);
                                    context = Some(CompletionContext::CellPath {
                                        prefix: cell_prefix,
                                        span: Span::new(cell_span_start, span.end),
                                        var_id,
                                        path_so_far,
                                    });
                                } else {
                                    let var_prefix = trimmed_prefix[1..].to_string();
                                    context = Some(CompletionContext::Variable {
                                        prefix: var_prefix,
                                        span,
                                    });
                                }
                            } else {
                                // Simple variable completion
                                let var_prefix = if trimmed_prefix.len() > 1 {
                                    trimmed_prefix[1..].to_string()
                                } else {
                                    String::new()
                                };
                                context = Some(CompletionContext::Variable {
                                    prefix: var_prefix,
                                    span,
                                });
                            }
                        } else if trimmed_prefix.starts_with('-') {
                            // This looks like a flag - find the command
                            if let Some((cmd_name, _)) = find_command_and_arg_index(idx, local_span)
                            {
                                context = Some(CompletionContext::Flag {
                                    prefix: trimmed_prefix.to_string(),
                                    span,
                                    command_name: cmd_name,
                                });
                            } else {
                                context = Some(CompletionContext::Argument { prefix, span });
                            }
                        } else {
                            // This is a positional argument - find the command and argument index
                            if let Some((cmd_name, arg_index)) =
                                find_command_and_arg_index(idx, local_span)
                            {
                                context = Some(CompletionContext::CommandArgument {
                                    prefix: trimmed_prefix.to_string(),
                                    span,
                                    command_name: cmd_name,
                                    arg_index,
                                });
                            } else {
                                context = Some(CompletionContext::Argument { prefix, span });
                            }
                        }
                    }
                }
            }
            break;
        }
    }

    // If not in a shape, check what comes before the cursor
    if context.is_none() {
        web_sys::console::log_1(&JsValue::from_str(
            "[completion] Context is None, entering fallback logic",
        ));
        // Check if there's a command-like shape before us
        let mut found_command_before = false;
        let mut has_separator_after_command = false;
        for (span, shape) in shapes.iter().rev() {
            let local_span = to_local_span(*span);
            if local_span.end <= byte_pos {
                if is_command_shape(shape, local_span) {
                    // Check if there's a pipe or semicolon between this command and the cursor
                    has_separator_after_command = has_separator_between(local_span.end, byte_pos);
                    web_sys::console::log_1(&JsValue::from_str(&format!(
                        "[completion] Found command shape {:?} at {:?}, has_separator_after_command={}",
                        shape, local_span, has_separator_after_command
                    )));
                    if !has_separator_after_command {
                        found_command_before = true;

                        // Extract the command text
                        let cmd = safe_slice(local_span);
                        web_sys::console::log_1(&JsValue::from_str(&format!(
                            "[completion] Set Command context with prefix: {:?}",
                            cmd
                        )));

                        // We're after a command, complete with that command as prefix
                        context = Some(CompletionContext::Command {
                            prefix: cmd,
                            span: local_span,
                        });
                    }
                }
                break;
            }
        }

        if !found_command_before {
            web_sys::console::log_1(&JsValue::from_str(
                "[completion] No command found before cursor, checking tokens",
            ));
            // No command before, check context from tokens
            let (tokens, _) = lex(input.as_bytes(), 0, &[], &[], true);
            let last_token = tokens.iter().filter(|t| t.span.end <= byte_pos).last();

            let is_cmd_context = if let Some(token) = last_token {
                let matches = matches!(
                    token.contents,
                    TokenContents::Pipe
                        | TokenContents::PipePipe
                        | TokenContents::Semicolon
                        | TokenContents::Eol
                );
                web_sys::console::log_1(&JsValue::from_str(&format!(
                    "[completion] Last token: {:?}, is_cmd_context from token={}",
                    token.contents, matches
                )));
                matches
            } else {
                web_sys::console::log_1(&JsValue::from_str(
                    "[completion] No last token found, assuming start of input (is_cmd_context=true)",
                ));
                true // Start of input
            };

            // Look for the last non-whitespace token before cursor
            let text_before = &input[..byte_pos];

            // Also check if we're inside a block - if the last non-whitespace char before cursor is '{'
            let text_before_trimmed = text_before.trim_end();
            let is_inside_block = text_before_trimmed.ends_with('{');
            // If we found a separator after a command, we're starting a new command
            let is_cmd_context = is_cmd_context || is_inside_block || has_separator_after_command;
            web_sys::console::log_1(&JsValue::from_str(&format!(
                "[completion] is_inside_block={}, has_separator_after_command={}, final is_cmd_context={}",
                is_inside_block, has_separator_after_command, is_cmd_context
            )));

            // Find the last word before cursor
            let last_word_start = text_before
                .rfind(|c: char| c.is_whitespace() || is_separator_char(c))
                .map(|i| i + 1)
                .unwrap_or(0);

            let last_word = text_before[last_word_start..].trim_start();
            web_sys::console::log_1(&JsValue::from_str(&format!(
                "[completion] last_word_start={}, last_word={:?}",
                last_word_start, last_word
            )));

            if is_cmd_context {
                context = Some(CompletionContext::Command {
                    prefix: last_word.to_string(),
                    span: Span::new(last_word_start, byte_pos),
                });
                web_sys::console::log_1(&JsValue::from_str(&format!(
                    "[completion] Set Command context with prefix: {:?}",
                    last_word
                )));
            } else {
                // Check if this is a variable or cell path (starts with $)
                let trimmed_word = last_word.trim();
                if trimmed_word.starts_with('$') {
                    // Check if this is a cell path (contains a dot after $)
                    if let Some(dot_pos) = trimmed_word[1..].find('.') {
                        // Cell path completion
                        let var_name = &trimmed_word[1..dot_pos + 1];
                        let after_var = &trimmed_word[dot_pos + 2..];
                        let parts: Vec<&str> = after_var.split('.').collect();
                        let (path_so_far, cell_prefix) = if parts.is_empty() {
                            (vec![], String::new())
                        } else if after_var.ends_with('.') {
                            (parts.iter().filter(|s| !s.is_empty()).map(|s| s.to_string()).collect(), String::new())
                        } else {
                            let path: Vec<String> = parts[..parts.len().saturating_sub(1)].iter().map(|s| s.to_string()).collect();
                            let prefix = parts.last().map(|s| s.to_string()).unwrap_or_default();
                            (path, prefix)
                        };
                        
                        let var_id = match var_name {
                            "env" => Some(ENV_VARIABLE_ID),
                            "nu" => Some(NU_VARIABLE_ID),
                            "in" => Some(IN_VARIABLE_ID),
                            _ => working_set.find_variable(var_name.as_bytes())
                        };
                        
                        if let Some(var_id) = var_id {
                            let prefix_byte_len = cell_prefix.len();
                            let cell_span_start = byte_pos.saturating_sub(prefix_byte_len);
                            let cell_prefix_clone = cell_prefix.clone();
                            context = Some(CompletionContext::CellPath {
                                prefix: cell_prefix,
                                span: Span::new(cell_span_start, byte_pos),
                                var_id,
                                path_so_far,
                            });
                            web_sys::console::log_1(&JsValue::from_str(&format!(
                                "[completion] Set CellPath context with prefix: {:?}",
                                cell_prefix_clone
                            )));
                        } else {
                            let var_prefix = trimmed_word[1..].to_string();
                            let var_prefix_clone = var_prefix.clone();
                            context = Some(CompletionContext::Variable {
                                prefix: var_prefix,
                                span: Span::new(last_word_start, byte_pos),
                            });
                            web_sys::console::log_1(&JsValue::from_str(&format!(
                                "[completion] Set Variable context with prefix: {:?}",
                                var_prefix_clone
                            )));
                        }
                    } else {
                        // Simple variable completion
                        let var_prefix = trimmed_word[1..].to_string();
                        let var_prefix_clone = var_prefix.clone();
                        context = Some(CompletionContext::Variable {
                            prefix: var_prefix,
                            span: Span::new(last_word_start, byte_pos),
                        });
                        web_sys::console::log_1(&JsValue::from_str(&format!(
                            "[completion] Set Variable context with prefix: {:?}",
                            var_prefix_clone
                        )));
                    }
                } else if trimmed_word.starts_with('-') {
                    // Try to find command by looking backwards through shapes
                    let mut found_cmd = None;
                    for (span, shape) in shapes.iter().rev() {
                        let local_span = to_local_span(*span);
                        if local_span.end <= byte_pos && is_command_shape(shape, local_span) {
                            let cmd_text = safe_slice(local_span);
                            let cmd_name = cmd_text
                                .split_whitespace()
                                .next()
                                .unwrap_or(&cmd_text)
                                .trim();
                            found_cmd = Some(cmd_name.to_string());
                            break;
                        }
                    }
                    if let Some(cmd_name) = found_cmd {
                        let cmd_name_clone = cmd_name.clone();
                        context = Some(CompletionContext::Flag {
                            prefix: trimmed_word.to_string(),
                            span: Span::new(last_word_start, byte_pos),
                            command_name: cmd_name,
                        });
                        web_sys::console::log_1(&JsValue::from_str(&format!(
                            "[completion] Set Flag context with prefix: {:?}, command: {:?}",
                            trimmed_word, cmd_name_clone
                        )));
                    } else {
                        context = Some(CompletionContext::Argument {
                            prefix: last_word.to_string(),
                            span: Span::new(last_word_start, byte_pos),
                        });
                        web_sys::console::log_1(&JsValue::from_str(&format!(
                            "[completion] Set Argument context with prefix: {:?}",
                            last_word
                        )));
                    }
                } else {
                    // Try to find command and argument index
                    let mut found_cmd = None;
                    let mut arg_count = 0;
                    for (span, shape) in shapes.iter().rev() {
                        let local_span = to_local_span(*span);
                        if local_span.end <= byte_pos {
                            if is_command_shape(shape, local_span) {
                                let cmd_text = safe_slice(local_span);
                                let cmd_name = cmd_text
                                    .split_whitespace()
                                    .next()
                                    .unwrap_or(&cmd_text)
                                    .trim();
                                found_cmd = Some(cmd_name.to_string());
                                break;
                            } else {
                                let arg_text = safe_slice(local_span);
                                let trimmed_arg = arg_text.trim();
                                if !trimmed_arg.is_empty() && !trimmed_arg.starts_with('-') {
                                    arg_count += 1;
                                }
                            }
                        }
                    }
                    if let Some(cmd_name) = found_cmd {
                        let cmd_name_clone = cmd_name.clone();
                        context = Some(CompletionContext::CommandArgument {
                            prefix: trimmed_word.to_string(),
                            span: Span::new(last_word_start, byte_pos),
                            command_name: cmd_name,
                            arg_index: arg_count,
                        });
                        web_sys::console::log_1(&JsValue::from_str(&format!(
                            "[completion] Set CommandArgument context with prefix: {:?}, command: {:?}, arg_index: {}",
                            trimmed_word, cmd_name_clone, arg_count
                        )));
                    } else {
                        context = Some(CompletionContext::Argument {
                            prefix: last_word.to_string(),
                            span: Span::new(last_word_start, byte_pos),
                        });
                        web_sys::console::log_1(&JsValue::from_str(&format!(
                            "[completion] Set Argument context with prefix: {:?}",
                            last_word
                        )));
                    }
                }
            }
        }
    }

    web_sys::console::log_1(&JsValue::from_str(&format!("context: {:?}", context)));

    let mut suggestions: Vec<Suggestion> = Vec::new();

    // Convert byte-spans back to char-spans for JS
    let to_char_span = |span: Span| -> Span {
        let char_start = input[..span.start].chars().count();
        let char_end = input[..span.end].chars().count();
        Span::new(char_start, char_end)
    };

    let get_command_signature = |cmd_name: &str| -> Option<nu_protocol::Signature> {
        engine_guard
            .find_decl(cmd_name.as_bytes(), &[])
            .map(|id| engine_guard.get_decl(id).signature())
    };

    match context {
        Some(CompletionContext::Command { prefix, span }) => {
            web_sys::console::log_1(&JsValue::from_str(&format!(
                "[completion] Generating Command suggestions with prefix: {:?}",
                prefix
            )));
            // Command completion
            let cmds = working_set
                .find_commands_by_predicate(|value| value.starts_with(prefix.as_bytes()), true);

            let span = to_char_span(span);
            let mut cmd_count = 0;

            for (_, name, desc, _) in cmds {
                let name_str = String::from_utf8_lossy(&name).to_string();
                suggestions.push(Suggestion {
                    rendered: {
                        let name_colored = ansi_term::Color::Green.bold().paint(&name_str);
                        let desc_str = desc.as_deref().unwrap_or("<no description>");
                        format!("{name_colored} {desc_str}")
                    },
                    name: name_str,
                    description: desc,
                    is_command: true,
                    span_start: span.start,
                    span_end: span.end,
                });
                cmd_count += 1;
            }
            web_sys::console::log_1(&JsValue::from_str(&format!(
                "[completion] Found {} command suggestions",
                cmd_count
            )));
        }
        Some(CompletionContext::Argument { prefix, span }) => {
            web_sys::console::log_1(&JsValue::from_str(&format!(
                "[completion] Generating Argument suggestions with prefix: {:?}",
                prefix
            )));
            // File completion
            let (dir, file_prefix) = prefix
                .rfind('/')
                .map(|idx| (&prefix[..idx + 1], &prefix[idx + 1..]))
                .unwrap_or(("", prefix.as_str()));

            let dir_to_join = (dir.len() > 1 && dir.ends_with('/'))
                .then(|| &dir[..dir.len() - 1])
                .unwrap_or(dir);

            let target_dir = if !dir.is_empty() {
                match root.join(dir_to_join) {
                    Ok(d) if d.is_dir().unwrap_or(false) => Some(d),
                    _ => None,
                }
            } else {
                Some(root.join("").unwrap())
            };

            let mut file_count = 0;
            if let Some(d) = target_dir {
                if let Ok(iterator) = d.read_dir() {
                    let span = to_char_span(span);

                    for entry in iterator {
                        let name = entry.filename();
                        if name.starts_with(file_prefix) {
                            let full_completion = format!("{}{}", dir, name);
                            suggestions.push(Suggestion {
                                name: full_completion.clone(),
                                description: None,
                                is_command: false,
                                rendered: full_completion,
                                span_start: span.start,
                                span_end: span.end,
                            });
                            file_count += 1;
                        }
                    }
                }
            }
            web_sys::console::log_1(&JsValue::from_str(&format!(
                "[completion] Found {} file suggestions",
                file_count
            )));
        }
        Some(CompletionContext::Flag {
            prefix,
            span,
            command_name,
        }) => {
            web_sys::console::log_1(&JsValue::from_str(&format!(
                "[completion] Generating Flag suggestions for command: {:?}, prefix: {:?}",
                command_name, prefix
            )));

            if let Some(signature) = get_command_signature(&command_name) {
                let span = to_char_span(span);
                let mut flag_count = 0;

                // Get switches from signature
                // Signature has a named field that contains named arguments (including switches)
                for flag in &signature.named {
                    // Check if this is a switch (has no argument)
                    // Switches have arg: None, named arguments have arg: Some(SyntaxShape)
                    let is_switch = flag.arg.is_none();

                    if is_switch {
                        let long_name = format!("--{}", flag.long);
                        let short_name = flag.short.map(|c| format!("-{}", c));

                        // Check if prefix matches long or short form
                        let matches_long = long_name.starts_with(&prefix) || prefix.is_empty();
                        let matches_short = short_name
                            .as_ref()
                            .map(|s| s.starts_with(&prefix) || prefix.is_empty())
                            .unwrap_or(false);

                        if matches_long {
                            suggestions.push(Suggestion {
                                name: long_name.clone(),
                                description: Some(flag.desc.clone()),
                                is_command: false,
                                rendered: {
                                    let flag_colored =
                                        ansi_term::Color::Cyan.bold().paint(&long_name);
                                    format!("{flag_colored} {}", flag.desc)
                                },
                                span_start: span.start,
                                span_end: span.end,
                            });
                            flag_count += 1;
                        }

                        if matches_short {
                            if let Some(short) = short_name {
                                suggestions.push(Suggestion {
                                    name: short.clone(),
                                    description: Some(flag.desc.clone()),
                                    is_command: false,
                                    rendered: {
                                        let flag_colored =
                                            ansi_term::Color::Cyan.bold().paint(&short);
                                        format!("{flag_colored} {}", flag.desc)
                                    },
                                    span_start: span.start,
                                    span_end: span.end,
                                });
                                flag_count += 1;
                            }
                        }
                    }
                }

                web_sys::console::log_1(&JsValue::from_str(&format!(
                    "[completion] Found {} flag suggestions",
                    flag_count
                )));
            } else {
                web_sys::console::log_1(&JsValue::from_str(&format!(
                    "[completion] Could not find signature for command: {:?}",
                    command_name
                )));
            }
        }
        Some(CompletionContext::CommandArgument {
            prefix,
            span,
            command_name,
            arg_index,
        }) => {
            web_sys::console::log_1(&JsValue::from_str(&format!(
                "[completion] Generating CommandArgument suggestions for command: {:?}, arg_index: {}, prefix: {:?}",
                command_name, arg_index, prefix
            )));

            if let Some(signature) = get_command_signature(&command_name) {
                // Get positional arguments from signature
                // Combine required and optional positional arguments
                let mut all_positional = Vec::new();
                all_positional.extend_from_slice(&signature.required_positional);
                all_positional.extend_from_slice(&signature.optional_positional);

                // Find the argument at the given index
                if let Some(arg) = all_positional.get(arg_index) {
                    // Check the SyntaxShape to determine completion type
                    match &arg.shape {
                        nu_protocol::SyntaxShape::String | nu_protocol::SyntaxShape::Filepath => {
                            // File/directory completion
                            let (dir, file_prefix) = prefix
                                .rfind('/')
                                .map(|idx| (&prefix[..idx + 1], &prefix[idx + 1..]))
                                .unwrap_or(("", prefix.as_str()));

                            let dir_to_join = (dir.len() > 1 && dir.ends_with('/'))
                                .then(|| &dir[..dir.len() - 1])
                                .unwrap_or(dir);

                            let target_dir = if !dir.is_empty() {
                                match root.join(dir_to_join) {
                                    Ok(d) if d.is_dir().unwrap_or(false) => Some(d),
                                    _ => None,
                                }
                            } else {
                                Some(root.join("").unwrap())
                            };

                            let span = to_char_span(span);
                            let mut file_count = 0;
                            if let Some(d) = target_dir {
                                if let Ok(iterator) = d.read_dir() {
                                    for entry in iterator {
                                        let name = entry.filename();
                                        if name.starts_with(file_prefix) {
                                            let full_completion = format!("{}{}", dir, name);
                                            suggestions.push(Suggestion {
                                                name: full_completion.clone(),
                                                description: Some(arg.desc.clone()),
                                                is_command: false,
                                                rendered: full_completion,
                                                span_start: span.start,
                                                span_end: span.end,
                                            });
                                            file_count += 1;
                                        }
                                    }
                                }
                            }
                            web_sys::console::log_1(&JsValue::from_str(&format!(
                                "[completion] Found {} file suggestions for argument {}",
                                file_count, arg_index
                            )));
                        }
                        _ => {
                            // For other types, fall back to file completion
                            let (dir, file_prefix) = prefix
                                .rfind('/')
                                .map(|idx| (&prefix[..idx + 1], &prefix[idx + 1..]))
                                .unwrap_or(("", prefix.as_str()));

                            let dir_to_join = (dir.len() > 1 && dir.ends_with('/'))
                                .then(|| &dir[..dir.len() - 1])
                                .unwrap_or(dir);

                            let target_dir = if !dir.is_empty() {
                                match root.join(dir_to_join) {
                                    Ok(d) if d.is_dir().unwrap_or(false) => Some(d),
                                    _ => None,
                                }
                            } else {
                                Some(root.join("").unwrap())
                            };

                            let span = to_char_span(span);
                            if let Some(d) = target_dir {
                                if let Ok(iterator) = d.read_dir() {
                                    for entry in iterator {
                                        let name = entry.filename();
                                        if name.starts_with(file_prefix) {
                                            let full_completion = format!("{}{}", dir, name);
                                            suggestions.push(Suggestion {
                                                name: full_completion.clone(),
                                                description: Some(arg.desc.clone()),
                                                is_command: false,
                                                rendered: full_completion,
                                                span_start: span.start,
                                                span_end: span.end,
                                            });
                                        }
                                    }
                                }
                            }
                        }
                    }
                } else {
                    // Argument index out of range, fall back to file completion
                    web_sys::console::log_1(&JsValue::from_str(&format!(
                        "[completion] Argument index {} out of range, using file completion",
                        arg_index
                    )));
                    // Use the same file completion logic as Argument context
                    let (dir, file_prefix) = prefix
                        .rfind('/')
                        .map(|idx| (&prefix[..idx + 1], &prefix[idx + 1..]))
                        .unwrap_or(("", prefix.as_str()));

                    let dir_to_join = (dir.len() > 1 && dir.ends_with('/'))
                        .then(|| &dir[..dir.len() - 1])
                        .unwrap_or(dir);

                    let target_dir = if !dir.is_empty() {
                        match root.join(dir_to_join) {
                            Ok(d) if d.is_dir().unwrap_or(false) => Some(d),
                            _ => None,
                        }
                    } else {
                        Some(root.join("").unwrap())
                    };

                    let span = to_char_span(span);
                    if let Some(d) = target_dir {
                        if let Ok(iterator) = d.read_dir() {
                            for entry in iterator {
                                let name = entry.filename();
                                if name.starts_with(file_prefix) {
                                    let full_completion = format!("{}{}", dir, name);
                                    suggestions.push(Suggestion {
                                        name: full_completion.clone(),
                                        description: None,
                                        is_command: false,
                                        rendered: full_completion,
                                        span_start: span.start,
                                        span_end: span.end,
                                    });
                                }
                            }
                        }
                    }
                }
            } else {
                // No signature found, fall back to file completion
                web_sys::console::log_1(&JsValue::from_str(&format!(
                    "[completion] Could not find signature for command: {:?}, using file completion",
                    command_name
                )));
                let (dir, file_prefix) = prefix
                    .rfind('/')
                    .map(|idx| (&prefix[..idx + 1], &prefix[idx + 1..]))
                    .unwrap_or(("", prefix.as_str()));

                let dir_to_join = (dir.len() > 1 && dir.ends_with('/'))
                    .then(|| &dir[..dir.len() - 1])
                    .unwrap_or(dir);

                let target_dir = if !dir.is_empty() {
                    match root.join(dir_to_join) {
                        Ok(d) if d.is_dir().unwrap_or(false) => Some(d),
                        _ => None,
                    }
                } else {
                    Some(root.join("").unwrap())
                };

                let span = to_char_span(span);
                if let Some(d) = target_dir {
                    if let Ok(iterator) = d.read_dir() {
                        for entry in iterator {
                            let name = entry.filename();
                            if name.starts_with(file_prefix) {
                                let full_completion = format!("{}{}", dir, name);
                                suggestions.push(Suggestion {
                                    name: full_completion.clone(),
                                    description: None,
                                    is_command: false,
                                    rendered: full_completion,
                                    span_start: span.start,
                                    span_end: span.end,
                                });
                            }
                        }
                    }
                }
            }
        }
        Some(CompletionContext::Variable { prefix, span }) => {
            web_sys::console::log_1(&JsValue::from_str(&format!(
                "[completion] Generating Variable suggestions with prefix: {:?}",
                prefix
            )));
            
            // Collect all available variables
            let variables = collect_variables(&working_set, &input, byte_pos);
            let span = to_char_span(span);
            let mut var_count = 0;
            
            for (var_name, var_id) in variables {
                // Filter by prefix (variable name includes $, so we need to check after $)
                if var_name.len() > 1 && var_name[1..].starts_with(&prefix) {
                    // Get variable type
                    let var_type = working_set.get_variable(var_id).ty.to_string();
                    
                    suggestions.push(Suggestion {
                        name: var_name.clone(),
                        description: Some(var_type.clone()),
                        is_command: false,
                        rendered: {
                            let var_colored = ansi_term::Color::Blue.bold().paint(&var_name);
                            format!("{var_colored} {var_type}")
                        },
                        span_start: span.start,
                        span_end: span.end,
                    });
                    var_count += 1;
                }
            }
            
            web_sys::console::log_1(&JsValue::from_str(&format!(
                "[completion] Found {} variable suggestions",
                var_count
            )));
        }
        Some(CompletionContext::CellPath { prefix, span, var_id, path_so_far }) => {
            web_sys::console::log_1(&JsValue::from_str(&format!(
                "[completion] Generating CellPath suggestions with prefix: {:?}, path: {:?}",
                prefix, path_so_far
            )));
            
            // Evaluate the variable to get its value
            if let Some(var_value) = eval_variable_for_completion(var_id, &working_set) {
                // Follow the path to get the value at the current level
                let current_value = if path_so_far.is_empty() {
                    var_value
                } else {
                    follow_cell_path(&var_value, &path_so_far).unwrap_or(var_value)
                };
                
                // Get columns/fields from the current value
                let columns = get_columns_from_value(&current_value);
                let span = to_char_span(span);
                let mut field_count = 0;
                
                for (col_name, col_type) in columns {
                    // Filter by prefix
                    if col_name.starts_with(&prefix) {
                        let type_str = col_type.as_deref().unwrap_or("any");
                        suggestions.push(Suggestion {
                            name: col_name.clone(),
                            description: Some(type_str.to_string()),
                            is_command: false,
                            rendered: {
                                let col_colored = ansi_term::Color::Yellow.paint(&col_name);
                                format!("{col_colored} {type_str}")
                            },
                            span_start: span.start,
                            span_end: span.end,
                        });
                        field_count += 1;
                    }
                }
                
                web_sys::console::log_1(&JsValue::from_str(&format!(
                    "[completion] Found {} cell path suggestions",
                    field_count
                )));
            } else {
                // Variable couldn't be evaluated - this is expected for runtime variables
                // We can't provide cell path completions without knowing the structure
                web_sys::console::log_1(&JsValue::from_str(&format!(
                    "[completion] Could not evaluate variable {:?} for cell path completion (runtime variable)",
                    var_id
                )));
                
                // Try to get type information to provide better feedback
                if let Ok(var_info) = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                    working_set.get_variable(var_id)
                })) {
                    web_sys::console::log_1(&JsValue::from_str(&format!(
                        "[completion] Variable type: {:?}",
                        var_info.ty
                    )));
                }
            }
        }
        _ => {
            web_sys::console::log_1(&JsValue::from_str(
                "[completion] Context is None, no suggestions generated",
            ));
        }
    }

    drop(working_set);
    drop(engine_guard);

    suggestions.sort();
    let suggestions = serde_json::to_string(&suggestions).unwrap_or_else(|_| "[]".to_string());
    web_sys::console::log_1(&JsValue::from_str(&suggestions));
    suggestions
}
