use crate::completion::helpers::*;
use crate::completion::types::{CompletionContext, CompletionKind};
use crate::console_log;
use nu_parser::FlatShape;
use nu_protocol::engine::{EngineState, StateWorkingSet};
use nu_protocol::{Signature, Span};

pub fn find_command_and_arg_index(
    input: &str,
    shapes: &[(Span, FlatShape)],
    current_idx: usize,
    current_local_span: Span,
    global_offset: usize,
) -> Option<(String, usize)> {
    let mut command_name: Option<String> = None;
    let mut arg_count = 0;

    // Look backwards through shapes to find the command
    for i in (0..current_idx).rev() {
        if let Some((prev_span, prev_shape)) = shapes.get(i) {
            let prev_local_span = to_local_span(*prev_span, global_offset);

            // Check if there's a separator between this shape and the next one
            let next_shape_start = if i + 1 < shapes.len() {
                to_local_span(shapes[i + 1].0, global_offset).start
            } else {
                current_local_span.start
            };

            if has_separator_between(input, prev_local_span.end, next_shape_start) {
                break; // Stop at separator
            }

            if is_command_shape(input, prev_shape, prev_local_span) {
                // Found the command
                let cmd_text = safe_slice(input, prev_local_span);
                let cmd_name = extract_command_name(cmd_text);
                command_name = Some(cmd_name.to_string());
                break;
            } else {
                // This is an argument - count it if it's not a flag
                let arg_text = safe_slice(input, prev_local_span);
                let trimmed_arg = arg_text.trim();
                // Don't count flags (starting with -) or empty arguments
                if !trimmed_arg.is_empty() && !trimmed_arg.starts_with('-') {
                    arg_count += 1;
                }
            }
        }
    }

    command_name.map(|name| (name, arg_count))
}

pub fn build_command_prefix(
    input: &str,
    shapes: &[(Span, FlatShape)],
    current_idx: usize,
    current_local_span: Span,
    current_prefix: &str,
    global_offset: usize,
) -> (String, Span) {
    let mut span_start = current_local_span.start;

    // Look backwards through shapes to find previous command words
    for i in (0..current_idx).rev() {
        if let Some((prev_span, prev_shape)) = shapes.get(i) {
            let prev_local_span = to_local_span(*prev_span, global_offset);

            if is_command_shape(input, prev_shape, prev_local_span) {
                // Check if there's a separator between this shape and the next one
                let next_shape_start = if i + 1 < shapes.len() {
                    to_local_span(shapes[i + 1].0, global_offset).start
                } else {
                    current_local_span.start
                };

                // Check if there's a separator (pipe, semicolon, etc.) between shapes
                // Whitespace is fine, but separators indicate a new command
                if has_separator_between(input, prev_local_span.end, next_shape_start) {
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
        safe_slice(input, Span::new(span_start, span_end)).to_string()
    } else {
        current_prefix.to_string()
    };

    (full_prefix, Span::new(span_start, span_end))
}

pub fn get_command_signature(engine_guard: &EngineState, cmd_name: &str) -> Option<Signature> {
    engine_guard
        .find_decl(cmd_name.as_bytes(), &[])
        .map(|id| engine_guard.get_decl(id).signature())
}

pub fn determine_flag_or_argument_context(
    input: &str,
    shapes: &[(Span, FlatShape)],
    prefix: &str,
    idx: usize,
    local_span: Span,
    span: Span,
    global_offset: usize,
) -> CompletionContext {
    let trimmed_prefix = prefix.trim();
    if trimmed_prefix.starts_with('-') {
        // This looks like a flag - find the command
        if let Some((cmd_name, _)) =
            find_command_and_arg_index(input, shapes, idx, local_span, global_offset)
        {
            CompletionContext {
                kind: CompletionKind::Flag {
                    command_name: cmd_name,
                },
                prefix: trimmed_prefix.to_string(),
                span,
            }
        } else {
            CompletionContext {
                kind: CompletionKind::Argument,
                prefix: prefix.to_string(),
                span,
            }
        }
    } else {
        // This is a positional argument - find the command and argument index
        if let Some((cmd_name, arg_index)) =
            find_command_and_arg_index(input, shapes, idx, local_span, global_offset)
        {
            CompletionContext {
                kind: CompletionKind::CommandArgument {
                    command_name: cmd_name,
                    arg_index,
                },
                prefix: trimmed_prefix.to_string(),
                span,
            }
        } else {
            CompletionContext {
                kind: CompletionKind::Argument,
                prefix: prefix.to_string(),
                span,
            }
        }
    }
}

pub fn handle_block_or_closure(
    input: &str,
    shapes: &[(Span, FlatShape)],
    working_set: &StateWorkingSet,
    prefix: &str,
    span: Span,
    shape_name: &str,
    current_idx: usize,
    local_span: Span,
    global_offset: usize,
) -> Option<CompletionContext> {
    console_log!("[completion] Processing {shape_name} shape with prefix: {prefix:?}");

    // Check if the content ends with a pipe or semicolon
    let prefix_ends_with_separator = ends_with_separator(prefix);
    let last_sep_pos_in_prefix = if prefix_ends_with_separator {
        find_last_separator_pos(prefix)
    } else {
        None
    };
    console_log!(
        "[completion] {shape_name}: prefix_ends_with_separator={prefix_ends_with_separator}, last_sep_pos_in_prefix={last_sep_pos_in_prefix:?}"
    );

    if let Some((trimmed_prefix, adjusted_span, is_empty)) = handle_block_prefix(prefix, span) {
        console_log!(
            "[completion] {shape_name}: trimmed_prefix={trimmed_prefix:?}, is_empty={is_empty}"
        );

        if is_empty {
            // Empty block/closure or just whitespace
            // Check if there's a command shape before this closure/block shape
            // If so, we might be completing after that command
            let mut found_command: Option<String> = None;
            for i in (0..current_idx).rev() {
                if let Some((prev_span, prev_shape)) = shapes.get(i) {
                    let prev_local_span = to_local_span(*prev_span, global_offset);
                    // Check if this shape is before the current closure and is a command
                    if prev_local_span.end <= local_span.start {
                        if is_command_shape(input, prev_shape, prev_local_span) {
                            let cmd_text = safe_slice(input, prev_local_span);
                            let cmd_full = cmd_text.trim().to_string();

                            // Extract the full command text - if it contains spaces, it might be a subcommand
                            // We'll use the first word for parent_command to show subcommands
                            // The suggestion generator will filter appropriately
                            let cmd_first_word = extract_command_name(cmd_text).to_string();

                            // If the command contains spaces, it's likely a full command (subcommand)
                            // In that case, we shouldn't show subcommands
                            if cmd_full.contains(' ') && cmd_full != cmd_first_word {
                                // It's a full command (subcommand), don't show subcommands
                                console_log!(
                                    "[completion] {shape_name} is empty but found full command {cmd_full:?} before it, not showing completions"
                                );
                                return None;
                            }

                            // Use the first word to show subcommands
                            found_command = Some(cmd_first_word);
                            console_log!(
                                "[completion] {shape_name} is empty but found command {found_command:?} before it"
                            );
                            break;
                        }
                    }
                }
            }

            if let Some(cmd_name) = found_command {
                // We found a command before the closure, show subcommands of that command
                console_log!(
                    "[completion] {shape_name} is empty, showing subcommands of {cmd_name:?}"
                );
                Some(CompletionContext {
                    kind: CompletionKind::Command {
                        parent_command: Some(cmd_name),
                    },
                    prefix: String::new(),
                    span: adjusted_span,
                })
            } else {
                // Truly empty - show all commands
                console_log!("[completion] {shape_name} is empty, setting Command context");
                Some(CompletionContext {
                    kind: CompletionKind::Command {
                        parent_command: None,
                    },
                    prefix: String::new(),
                    span: adjusted_span,
                })
            }
        } else if let Some(last_sep_pos) = last_sep_pos_in_prefix {
            // After a separator - command context
            let after_sep = prefix[last_sep_pos..].trim_start();
            console_log!(
                "[completion] {shape_name} has separator at {last_sep_pos}, after_sep={after_sep:?}, setting Command context"
            );
            Some(CompletionContext {
                kind: CompletionKind::Command {
                    parent_command: None,
                },
                prefix: after_sep.to_string(),
                span: Span::new(span.start + last_sep_pos, span.end),
            })
        } else {
            console_log!(
                "[completion] {shape_name} has no separator, checking for variable/flag/argument context"
            );
            // Check if this is a variable or cell path first
            let trimmed = trimmed_prefix.trim();

            if trimmed.starts_with('$') {
                // Variable or cell path completion
                if let Some((var_name, path_so_far, cell_prefix)) = parse_cell_path(trimmed) {
                    let var_id = lookup_variable_id(var_name, working_set);

                    if let Some(var_id) = var_id {
                        let prefix_byte_len = cell_prefix.len();
                        let cell_span_start = adjusted_span.end.saturating_sub(prefix_byte_len);
                        console_log!(
                            "[completion] {shape_name}: Setting CellPath context with var {var_name:?}, prefix {cell_prefix:?}"
                        );
                        Some(CompletionContext {
                            kind: CompletionKind::CellPath {
                                var_id,
                                path_so_far: path_so_far.iter().map(|s| s.to_string()).collect(),
                            },
                            prefix: cell_prefix.to_string(),
                            span: Span::new(cell_span_start, adjusted_span.end),
                        })
                    } else {
                        // Unknown variable, fall back to variable completion
                        let var_prefix = trimmed[1..].to_string();
                        console_log!(
                            "[completion] {shape_name}: Unknown var, setting Variable context with prefix {var_prefix:?}"
                        );
                        Some(CompletionContext {
                            kind: CompletionKind::Variable,
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
                    console_log!(
                        "[completion] {shape_name}: Setting Variable context with prefix {var_prefix:?}"
                    );
                    Some(CompletionContext {
                        kind: CompletionKind::Variable,
                        prefix: var_prefix,
                        span: adjusted_span,
                    })
                }
            } else if trimmed.starts_with('-') {
                // Flag completion
                if let Some((cmd_name, _)) = find_command_and_arg_index(
                    input,
                    shapes,
                    current_idx,
                    local_span,
                    global_offset,
                ) {
                    console_log!(
                        "[completion] {shape_name}: Found command {cmd_name:?} for flag completion"
                    );
                    Some(CompletionContext {
                        kind: CompletionKind::Flag {
                            command_name: cmd_name,
                        },
                        prefix: trimmed.to_string(),
                        span: adjusted_span,
                    })
                } else {
                    Some(CompletionContext {
                        kind: CompletionKind::Argument,
                        prefix: trimmed_prefix.to_string(),
                        span: adjusted_span,
                    })
                }
            } else {
                // Try to find the command and argument index
                if let Some((cmd_name, arg_index)) = find_command_and_arg_index(
                    input,
                    shapes,
                    current_idx,
                    local_span,
                    global_offset,
                ) {
                    console_log!(
                        "[completion] {shape_name}: Found command {cmd_name:?} with arg_index {arg_index} for argument completion"
                    );
                    Some(CompletionContext {
                        kind: CompletionKind::CommandArgument {
                            command_name: cmd_name,
                            arg_index,
                        },
                        prefix: trimmed.to_string(),
                        span: adjusted_span,
                    })
                } else {
                    // No command found, treat as regular argument
                    console_log!(
                        "[completion] {shape_name}: No command found, using Argument context"
                    );
                    Some(CompletionContext {
                        kind: CompletionKind::Argument,
                        prefix: trimmed_prefix.to_string(),
                        span: adjusted_span,
                    })
                }
            }
        }
    } else {
        None
    }
}

pub fn handle_variable_string_shape(
    input: &str,
    shapes: &[(Span, FlatShape)],
    _working_set: &StateWorkingSet,
    idx: usize,
    prefix: &str,
    span: Span,
    local_span: Span,
    global_offset: usize,
) -> Option<CompletionContext> {
    if idx == 0 {
        return None;
    }

    let prev_shape = &shapes[idx - 1];
    let prev_local_span = to_local_span(prev_shape.0, global_offset);

    if let FlatShape::Variable(var_id) = prev_shape.1 {
        // Check if the variable shape ends right where this shape starts (or very close)
        // Allow for a small gap (like a dot) between shapes
        let gap = local_span.start.saturating_sub(prev_local_span.end);
        if gap <= 1 {
            // This is a cell path - the String shape contains the field name(s)
            // The prefix might be like "na" or "field.subfield"
            let trimmed_prefix = prefix.trim();
            let (path_so_far, cell_prefix) = parse_cell_path_from_fields(trimmed_prefix);

            let prefix_byte_len = cell_prefix.len();
            let cell_span_start = span.end.saturating_sub(prefix_byte_len);
            console_log!(
                "[completion] Detected cell path from Variable+String shapes, var_id={var_id:?}, prefix={cell_prefix:?}, path={path_so_far:?}"
            );
            Some(CompletionContext {
                kind: CompletionKind::CellPath {
                    var_id,
                    path_so_far: path_so_far.iter().map(|s| s.to_string()).collect(),
                },
                prefix: cell_prefix.to_string(),
                span: Span::new(cell_span_start, span.end),
            })
        } else {
            // Gap between shapes, use helper to determine context
            Some(determine_flag_or_argument_context(
                input,
                shapes,
                &prefix.trim(),
                idx,
                local_span,
                span,
                global_offset,
            ))
        }
    } else {
        // Previous shape is not a Variable, use helper to determine context
        Some(determine_flag_or_argument_context(
            input,
            shapes,
            &prefix.trim(),
            idx,
            local_span,
            span,
            global_offset,
        ))
    }
}

pub fn handle_dot_shape(
    _input: &str,
    shapes: &[(Span, FlatShape)],
    idx: usize,
    prefix: &str,
    span: Span,
    local_span: Span,
    global_offset: usize,
) -> Option<CompletionContext> {
    if idx == 0 {
        return Some(CompletionContext {
            kind: CompletionKind::Argument,
            prefix: prefix.to_string(),
            span,
        });
    }

    let prev_shape = &shapes[idx - 1];
    let prev_local_span = to_local_span(prev_shape.0, global_offset);

    if let FlatShape::Variable(var_id) = prev_shape.1 {
        // Check if the variable shape ends right where this shape starts
        if prev_local_span.end == local_span.start {
            let trimmed_prefix = prefix.trim();
            // Parse path members from the prefix (which is like ".field" or ".field.subfield")
            let after_dot = &trimmed_prefix[1..]; // Remove leading dot
            let (path_so_far, cell_prefix) = if after_dot.is_empty() {
                (vec![], "")
            } else {
                parse_cell_path_from_fields(after_dot)
            };

            let prefix_byte_len = cell_prefix.len();
            let cell_span_start = span.end.saturating_sub(prefix_byte_len);
            console_log!(
                "[completion] Detected cell path from adjacent Variable shape, var_id={var_id:?}, prefix={cell_prefix:?}"
            );
            Some(CompletionContext {
                kind: CompletionKind::CellPath {
                    var_id,
                    path_so_far: path_so_far.iter().map(|s| s.to_string()).collect(),
                },
                prefix: cell_prefix.to_string(),
                span: Span::new(cell_span_start, span.end),
            })
        } else {
            // Gap between shapes, fall through to default handling
            Some(CompletionContext {
                kind: CompletionKind::Argument,
                prefix: prefix.to_string(),
                span,
            })
        }
    } else {
        // Previous shape is not a Variable, this is likely a file path starting with .
        Some(CompletionContext {
            kind: CompletionKind::Argument,
            prefix: prefix.to_string(),
            span,
        })
    }
}

pub fn determine_context_from_shape(
    input: &str,
    shapes: &[(Span, FlatShape)],
    working_set: &StateWorkingSet,
    byte_pos: usize,
    global_offset: usize,
) -> Option<CompletionContext> {
    // First, check if cursor is within a shape
    for (idx, (span, shape)) in shapes.iter().enumerate() {
        let local_span = to_local_span(*span, global_offset);

        if local_span.start <= byte_pos && byte_pos <= local_span.end {
            console_log!("[completion] Cursor in shape {idx}: {shape:?} at {local_span:?}");

            // Check if there's a pipe or semicolon between this shape's end and the cursor
            // If so, we're starting a new command and should ignore this shape
            let has_sep = has_separator_between(input, local_span.end, byte_pos);
            if has_sep {
                console_log!(
                    "[completion] Separator found between shape end ({end}) and cursor ({byte_pos}), skipping shape",
                    end = local_span.end
                );
                // There's a separator, so we're starting a new command - skip this shape
                continue;
            }

            let span = Span::new(local_span.start, std::cmp::min(local_span.end, byte_pos));
            let prefix = safe_slice(input, span);
            console_log!("[completion] Processing shape {idx} with prefix: {prefix:?}");

            // Special case: if prefix is just '{' (possibly with whitespace),
            // we're at the start of a block and should complete commands
            let trimmed_prefix = prefix.trim();
            if trimmed_prefix == "{" {
                // We're right after '{' - command context
                if let Some((_, adjusted_span, _)) = handle_block_prefix(&prefix, span) {
                    return Some(CompletionContext {
                        kind: CompletionKind::Command {
                            parent_command: None,
                        },
                        prefix: String::new(),
                        span: adjusted_span,
                    });
                }
            } else {
                match shape {
                    // Special case: Check if we're completing a cell path where the Variable and field are in separate shapes
                    _ if { idx > 0 && matches!(shape, FlatShape::String) } => {
                        if let Some(ctx) = handle_variable_string_shape(
                            input,
                            shapes,
                            working_set,
                            idx,
                            &prefix,
                            span,
                            local_span,
                            global_offset,
                        ) {
                            return Some(ctx);
                        }
                    }
                    // Special case: Check if we're completing a cell path where the Variable and dot are in separate shapes
                    _ if {
                        let trimmed_prefix = prefix.trim();
                        trimmed_prefix.starts_with('.') && idx > 0
                    } =>
                    {
                        if let Some(ctx) = handle_dot_shape(
                            input,
                            shapes,
                            idx,
                            &prefix,
                            span,
                            local_span,
                            global_offset,
                        ) {
                            return Some(ctx);
                        }
                    }
                    _ if {
                        // Check if this is a variable or cell path (starts with $) before treating as command
                        let trimmed_prefix = prefix.trim();
                        trimmed_prefix.starts_with('$')
                    } =>
                    {
                        let trimmed_prefix = prefix.trim();
                        // Check if this is a cell path (contains a dot after $)
                        if let Some((var_name, path_so_far, cell_prefix)) =
                            parse_cell_path(trimmed_prefix)
                        {
                            // Find the variable ID
                            let var_id = lookup_variable_id(var_name, working_set);

                            if let Some(var_id) = var_id {
                                // Calculate span for the cell path member being completed
                                let prefix_byte_len = cell_prefix.len();
                                let cell_span_start = span.end.saturating_sub(prefix_byte_len);
                                return Some(CompletionContext {
                                    kind: CompletionKind::CellPath {
                                        var_id,
                                        path_so_far: path_so_far
                                            .iter()
                                            .map(|s| s.to_string())
                                            .collect(),
                                    },
                                    prefix: cell_prefix.to_string(),
                                    span: Span::new(cell_span_start, span.end),
                                });
                            } else {
                                // Unknown variable, fall back to variable completion
                                let var_prefix = trimmed_prefix[1..].to_string();
                                return Some(CompletionContext {
                                    kind: CompletionKind::Variable,
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
                            return Some(CompletionContext {
                                kind: CompletionKind::Variable,
                                prefix: var_prefix,
                                span,
                            });
                        }
                    }
                    _ if is_command_shape(input, shape, local_span) => {
                        let (full_prefix, full_span) =
                            build_command_prefix(input, shapes, idx, span, &prefix, global_offset);
                        return Some(CompletionContext {
                            kind: CompletionKind::Command {
                                parent_command: None,
                            },
                            prefix: full_prefix,
                            span: full_span,
                        });
                    }
                    FlatShape::Block | FlatShape::Closure => {
                        if let Some(ctx) = handle_block_or_closure(
                            input,
                            shapes,
                            working_set,
                            &prefix,
                            span,
                            shape.as_str().trim_start_matches("shape_"),
                            idx,
                            local_span,
                            global_offset,
                        ) {
                            return Some(ctx);
                        }
                    }
                    FlatShape::Variable(var_id) => {
                        // Variable or cell path completion context
                        let trimmed_prefix = prefix.trim();
                        if trimmed_prefix.starts_with('$') {
                            // Check if this is a cell path (contains a dot after $)
                            if let Some((_, path_so_far, cell_prefix)) =
                                parse_cell_path(trimmed_prefix)
                            {
                                let prefix_byte_len = cell_prefix.len();
                                let cell_span_start = span.end.saturating_sub(prefix_byte_len);
                                return Some(CompletionContext {
                                    kind: CompletionKind::CellPath {
                                        var_id: *var_id,
                                        path_so_far: path_so_far
                                            .iter()
                                            .map(|s| s.to_string())
                                            .collect(),
                                    },
                                    prefix: cell_prefix.to_string(),
                                    span: Span::new(cell_span_start, span.end),
                                });
                            } else {
                                // Simple variable completion
                                let var_prefix = trimmed_prefix[1..].to_string();
                                return Some(CompletionContext {
                                    kind: CompletionKind::Variable,
                                    prefix: var_prefix,
                                    span,
                                });
                            }
                        } else {
                            // Fallback to argument context if no $ found
                            return Some(CompletionContext {
                                kind: CompletionKind::Argument,
                                prefix: prefix.to_string(),
                                span,
                            });
                        }
                    }
                    _ => {
                        // Check if this is a variable or cell path (starts with $)
                        let trimmed_prefix = prefix.trim();
                        if trimmed_prefix.starts_with('$') {
                            // Check if this is a cell path (contains a dot after $)
                            if let Some((var_name, path_so_far, cell_prefix)) =
                                parse_cell_path(trimmed_prefix)
                            {
                                let var_id = lookup_variable_id(var_name, working_set);
                                if let Some(var_id) = var_id {
                                    let prefix_byte_len = cell_prefix.len();
                                    let cell_span_start = span.end.saturating_sub(prefix_byte_len);
                                    return Some(CompletionContext {
                                        kind: CompletionKind::CellPath {
                                            var_id,
                                            path_so_far: path_so_far
                                                .iter()
                                                .map(|s| s.to_string())
                                                .collect(),
                                        },
                                        prefix: cell_prefix.to_string(),
                                        span: Span::new(cell_span_start, span.end),
                                    });
                                } else {
                                    let var_prefix = trimmed_prefix[1..].to_string();
                                    return Some(CompletionContext {
                                        kind: CompletionKind::Variable,
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
                                return Some(CompletionContext {
                                    kind: CompletionKind::Variable,
                                    prefix: var_prefix,
                                    span,
                                });
                            }
                        } else {
                            // Use helper to determine flag or argument context
                            return Some(determine_flag_or_argument_context(
                                input,
                                shapes,
                                &trimmed_prefix,
                                idx,
                                local_span,
                                span,
                                global_offset,
                            ));
                        }
                    }
                }
            }
            break;
        }
    }
    None
}

pub fn determine_context_fallback(
    input: &str,
    shapes: &[(Span, FlatShape)],
    working_set: &StateWorkingSet,
    engine_guard: &EngineState,
    byte_pos: usize,
    global_offset: usize,
) -> Vec<CompletionContext> {
    use nu_parser::{TokenContents, lex};

    console_log!("[completion] Context is None, entering fallback logic");
    // Check if there's a command-like shape before us
    let mut has_separator_after_command = false;
    for (span, shape) in shapes.iter().rev() {
        let local_span = to_local_span(*span, global_offset);
        if local_span.end <= byte_pos {
            if is_command_shape(input, shape, local_span) {
                // Check if there's a pipe or semicolon between this command and the cursor
                has_separator_after_command =
                    has_separator_between(input, local_span.end, byte_pos);
                console_log!(
                    "[completion] Found command shape {shape:?} at {local_span:?}, has_separator_after_command={has_separator_after_command}"
                );
                if !has_separator_after_command {
                    // Extract the command text (full command including subcommands)
                    let cmd = safe_slice(input, local_span);
                    let cmd_full = cmd.trim().to_string();
                    let cmd_first_word = extract_command_name(cmd).to_string();

                    // Check if we're right after the command (only whitespace between command and cursor)
                    let text_after_command = if local_span.end < input.len() {
                        &input[local_span.end..byte_pos]
                    } else {
                        ""
                    };
                    let is_right_after_command = text_after_command.trim().is_empty();

                    // If we're right after a command, check if it has positional arguments
                    if is_right_after_command {
                        // Check if the command text contains spaces (indicating it's a subcommand like "attr category")
                        let is_subcommand = cmd_full.contains(' ') && cmd_full != cmd_first_word;

                        // First, try the full command name (e.g., "attr category")
                        // If that doesn't exist, fall back to the first word (e.g., "attr")
                        let full_cmd_exists =
                            get_command_signature(engine_guard, &cmd_full).is_some();
                        let cmd_name = if full_cmd_exists {
                            cmd_full.clone()
                        } else {
                            cmd_first_word.clone()
                        };

                        let mut context = Vec::with_capacity(2);
                        if let Some(signature) = get_command_signature(engine_guard, &cmd_name) {
                            // Check if command has any positional arguments
                            let has_positional_args = !signature.required_positional.is_empty()
                                || !signature.optional_positional.is_empty();

                            if has_positional_args {
                                // Count existing arguments before cursor
                                let mut arg_count = 0;
                                for (prev_span, prev_shape) in shapes.iter().rev() {
                                    let prev_local_span = to_local_span(*prev_span, global_offset);
                                    if prev_local_span.end <= byte_pos
                                        && prev_local_span.end > local_span.end
                                    {
                                        if !is_command_shape(input, prev_shape, prev_local_span) {
                                            let arg_text = safe_slice(input, prev_local_span);
                                            let trimmed_arg = arg_text.trim();
                                            // Don't count flags (starting with -) or empty arguments
                                            if !trimmed_arg.is_empty()
                                                && !trimmed_arg.starts_with('-')
                                            {
                                                arg_count += 1;
                                            }
                                        }
                                    }
                                }

                                console_log!(
                                    "[completion] Right after command {cmd_name:?}, setting CommandArgument context with arg_index: {arg_count}"
                                );

                                context.push(CompletionContext {
                                    kind: CompletionKind::CommandArgument {
                                        command_name: cmd_name.clone(),
                                        arg_index: arg_count,
                                    },
                                    prefix: String::new(),
                                    span: Span::new(byte_pos, byte_pos),
                                });
                            }
                        }
                        // No positional arguments
                        // If this is a subcommand (contains spaces), don't show subcommands
                        // Only show subcommands if we're using just the base command (single word)
                        if is_subcommand && full_cmd_exists {
                            console_log!(
                                "[completion] Command {cmd_name:?} is a subcommand with no positional args, not showing completions"
                            );
                        } else {
                            // Show subcommands of the base command
                            console_log!(
                                "[completion] Command {cmd_name:?} has no positional args, showing subcommands"
                            );
                            context.push(CompletionContext {
                                kind: CompletionKind::Command {
                                    parent_command: Some(cmd_first_word),
                                },
                                prefix: String::new(),
                                span: Span::new(byte_pos, byte_pos),
                            });
                        }
                        // reverse to put subcommands in the beginning
                        context.reverse();
                        return context;
                    } else {
                        // Not right after command, complete the command itself
                        console_log!("[completion] Set Command context with prefix: {cmd:?}");
                        return vec![CompletionContext {
                            kind: CompletionKind::Command {
                                parent_command: None,
                            },
                            prefix: cmd.to_string(),
                            span: local_span,
                        }];
                    }
                }
            }
            break;
        }
    }

    // No command found before, check context from tokens
    console_log!("[completion] No command found before cursor, checking tokens");
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
        console_log!(
            "[completion] Last token: {contents:?}, is_cmd_context from token={matches}",
            contents = token.contents
        );
        matches
    } else {
        console_log!(
            "[completion] No last token found, assuming start of input (is_cmd_context=true)"
        );
        true // Start of input
    };

    // Look for the last non-whitespace token before cursor
    let text_before = &input[..byte_pos];

    // Also check if we're inside a block - if the last non-whitespace char before cursor is '{'
    let text_before_trimmed = text_before.trim_end();
    let is_inside_block = text_before_trimmed.ends_with('{');
    // If we found a separator after a command, we're starting a new command
    let is_cmd_context = is_cmd_context || is_inside_block || has_separator_after_command;
    console_log!(
        "[completion] is_inside_block={is_inside_block}, has_separator_after_command={has_separator_after_command}, final is_cmd_context={is_cmd_context}"
    );

    // Find the last word before cursor
    let last_word_start = text_before
        .rfind(|c: char| c.is_whitespace() || is_separator_char(c))
        .map(|i| i + 1)
        .unwrap_or(0);

    let last_word = text_before[last_word_start..].trim_start();
    console_log!("[completion] last_word_start={last_word_start}, last_word={last_word:?}");

    if is_cmd_context {
        vec![CompletionContext {
            kind: CompletionKind::Command {
                parent_command: None,
            },
            prefix: last_word.to_string(),
            span: Span::new(last_word_start, byte_pos),
        }]
    } else {
        // Check if this is a variable or cell path (starts with $)
        let trimmed_word = last_word.trim();
        if trimmed_word.starts_with('$') {
            // Check if this is a cell path (contains a dot after $)
            if let Some((var_name, path_so_far, cell_prefix)) = parse_cell_path(trimmed_word) {
                let var_id = lookup_variable_id(&var_name, working_set);

                if let Some(var_id) = var_id {
                    let prefix_byte_len = cell_prefix.len();
                    let cell_span_start = byte_pos.saturating_sub(prefix_byte_len);
                    vec![CompletionContext {
                        kind: CompletionKind::CellPath {
                            var_id,
                            path_so_far: path_so_far.iter().map(|s| s.to_string()).collect(),
                        },
                        prefix: cell_prefix.to_string(),
                        span: Span::new(cell_span_start, byte_pos),
                    }]
                } else {
                    let var_prefix = trimmed_word[1..].to_string();
                    vec![CompletionContext {
                        kind: CompletionKind::Variable,
                        prefix: var_prefix,
                        span: Span::new(last_word_start, byte_pos),
                    }]
                }
            } else {
                // Simple variable completion
                let var_prefix = trimmed_word[1..].to_string();
                vec![CompletionContext {
                    kind: CompletionKind::Variable,
                    prefix: var_prefix,
                    span: Span::new(last_word_start, byte_pos),
                }]
            }
        } else if trimmed_word.starts_with('-') {
            // Try to find command by looking backwards through shapes
            let mut found_cmd = None;
            for (span, shape) in shapes.iter().rev() {
                let local_span = to_local_span(*span, global_offset);
                if local_span.end <= byte_pos && is_command_shape(input, shape, local_span) {
                    let cmd_text = safe_slice(input, local_span);
                    let cmd_name = extract_command_name(cmd_text).to_string();
                    found_cmd = Some(cmd_name);
                    break;
                }
            }
            if let Some(cmd_name) = found_cmd {
                vec![CompletionContext {
                    kind: CompletionKind::Flag {
                        command_name: cmd_name,
                    },
                    prefix: trimmed_word.to_string(),
                    span: Span::new(last_word_start, byte_pos),
                }]
            } else {
                vec![CompletionContext {
                    kind: CompletionKind::Argument,
                    prefix: last_word.to_string(),
                    span: Span::new(last_word_start, byte_pos),
                }]
            }
        } else {
            // Try to find command and argument index
            let mut found_cmd = None;
            let mut arg_count = 0;
            for (span, shape) in shapes.iter().rev() {
                let local_span = to_local_span(*span, global_offset);
                if local_span.end <= byte_pos {
                    if is_command_shape(input, shape, local_span) {
                        let cmd_text = safe_slice(input, local_span);
                        let cmd_name = extract_command_name(cmd_text).to_string();
                        found_cmd = Some(cmd_name);
                        break;
                    } else {
                        let arg_text = safe_slice(input, local_span);
                        let trimmed_arg = arg_text.trim();
                        if !trimmed_arg.is_empty() && !trimmed_arg.starts_with('-') {
                            arg_count += 1;
                        }
                    }
                }
            }
            if let Some(cmd_name) = found_cmd {
                vec![CompletionContext {
                    kind: CompletionKind::CommandArgument {
                        command_name: cmd_name,
                        arg_index: arg_count,
                    },
                    prefix: trimmed_word.to_string(),
                    span: Span::new(last_word_start, byte_pos),
                }]
            } else {
                vec![CompletionContext {
                    kind: CompletionKind::Argument,
                    prefix: last_word.to_string(),
                    span: Span::new(last_word_start, byte_pos),
                }]
            }
        }
    }
}

pub fn determine_context(
    input: &str,
    shapes: &[(Span, FlatShape)],
    working_set: &StateWorkingSet,
    engine_guard: &EngineState,
    byte_pos: usize,
    global_offset: usize,
) -> Vec<CompletionContext> {
    // First try to determine context from shapes
    if let Some(ctx) =
        determine_context_from_shape(input, shapes, working_set, byte_pos, global_offset)
    {
        return vec![ctx];
    }

    // Fallback to token-based context determination
    determine_context_fallback(
        input,
        shapes,
        working_set,
        engine_guard,
        byte_pos,
        global_offset,
    )
}
