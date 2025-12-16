use futures::FutureExt;
use js_sys::Promise;
use wasm_bindgen_futures::future_to_promise;

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

    // Helper function to handle both Block and Closure shapes
    let handle_block_or_closure = |prefix: &str,
                                   span: Span,
                                   shape_name: &str|
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
                    "[completion] {} has no separator, setting Argument context",
                    shape_name
                )));
                Some(CompletionContext::Argument {
                    prefix: trimmed_prefix,
                    span: adjusted_span,
                })
            }
        } else {
            None
        }
    };

    // Find what we're completing
    #[derive(Debug)]
    enum CompletionContext {
        Command { prefix: String, span: Span },
        Argument { prefix: String, span: Span },
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
                        ) {
                            context = Some(ctx);
                        }
                    }
                    _ => {
                        context = Some(CompletionContext::Argument { prefix, span });
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

    web_sys::console::log_1(&JsValue::from_str(&format!("context: {:?}", context)));

    let mut suggestions: Vec<Suggestion> = Vec::new();

    // Convert byte-spans back to char-spans for JS
    let to_char_span = |span: Span| -> Span {
        let char_start = input[..span.start].chars().count();
        let char_end = input[..span.end].chars().count();
        Span::new(char_start, char_end)
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
