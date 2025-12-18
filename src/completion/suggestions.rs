use crate::completion::context::get_command_signature;
use crate::completion::helpers::to_char_span;
use crate::completion::types::{CompletionContext, Suggestion};
use crate::completion::variables::*;
use crate::console_log;
use nu_protocol::Span;
use nu_protocol::engine::{EngineState, Stack, StateWorkingSet};

pub fn generate_command_suggestions(
    input: &str,
    working_set: &StateWorkingSet,
    prefix: String,
    span: Span,
    parent_command: Option<String>,
) -> Vec<Suggestion> {
    console_log!(
        "[completion] Generating Command suggestions with prefix: {prefix:?}, parent_command: {parent_command:?}"
    );

    let span = to_char_span(input, span);
    let mut suggestions = Vec::new();
    let mut cmd_count = 0;

    // Determine search prefix and name extraction logic
    let (search_prefix, parent_prefix_opt) = if let Some(parent) = &parent_command {
        // Show only subcommands of the parent command
        // Subcommands are commands that start with "parent_command " (with space)
        let parent_prefix = format!("{} ", parent);
        let search_prefix = if prefix.is_empty() {
            parent_prefix.clone()
        } else {
            format!("{}{}", parent_prefix, prefix)
        };
        (search_prefix, Some(parent_prefix))
    } else {
        // Regular command completion - show all commands
        (prefix.clone(), None)
    };

    let cmds = working_set
        .find_commands_by_predicate(|value| value.starts_with(search_prefix.as_bytes()), true);

    for (_, name, desc, _) in cmds {
        let name_str = String::from_utf8_lossy(&name).to_string();

        // Extract the command name to display
        // For subcommands, extract just the subcommand name (part after "parent_command ")
        // For regular commands, use the full command name
        let display_name = if let Some(parent_prefix) = &parent_prefix_opt {
            if let Some(subcommand_name) = name_str.strip_prefix(parent_prefix) {
                subcommand_name.to_string()
            } else {
                continue; // Skip if it doesn't match the parent prefix
            }
        } else {
            name_str
        };

        suggestions.push(Suggestion {
            rendered: {
                let name_colored = ansi_term::Color::Green.bold().paint(&display_name);
                let desc_str = desc.as_deref().unwrap_or("<no description>");
                format!("{name_colored} {desc_str}")
            },
            name: display_name,
            description: desc.map(|d| d.to_string()),
            span_start: span.start,
            span_end: span.end,
        });
        cmd_count += 1;
    }
    console_log!("[completion] Found {cmd_count} command suggestions");
    suggestions.sort();
    suggestions
}

pub fn generate_argument_suggestions(
    input: &str,
    prefix: String,
    span: Span,
    root: &std::sync::Arc<vfs::VfsPath>,
) -> Vec<Suggestion> {
    console_log!("[completion] Generating Argument suggestions with prefix: {prefix:?}");
    // File completion
    let mut file_suggestions = generate_file_suggestions(&prefix, span, root, None, input);
    console_log!(
        "[completion] Found {file_count} file suggestions",
        file_count = file_suggestions.len()
    );
    file_suggestions.sort();
    file_suggestions
}

pub fn generate_flag_suggestions(
    input: &str,
    engine_guard: &EngineState,
    prefix: String,
    span: Span,
    command_name: String,
) -> Vec<Suggestion> {
    console_log!(
        "[completion] Generating Flag suggestions for command: {command_name:?}, prefix: {prefix:?}"
    );

    let mut suggestions = Vec::new();
    if let Some(signature) = get_command_signature(engine_guard, &command_name) {
        let span = to_char_span(input, span);
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

                // Determine which flags to show based on prefix:
                // - If prefix is empty or exactly "-", show all flags (both short and long)
                // - If prefix starts with "--", only show long flags that match the prefix
                // - If prefix starts with "-" (but not "--"), only show short flags that match the prefix
                let show_all = prefix.is_empty() || prefix == "-";

                // Helper to create a flag suggestion
                let create_flag_suggestion = |flag_name: String| -> Suggestion {
                    Suggestion {
                        name: flag_name.clone(),
                        description: Some(flag.desc.clone()),
                        rendered: {
                            let flag_colored = ansi_term::Color::Cyan.bold().paint(&flag_name);
                            format!("{flag_colored} {}", flag.desc)
                        },
                        span_start: span.start,
                        span_end: span.end,
                    }
                };

                // Add long flag if it matches
                let should_show_long = if show_all {
                    true // Show all flags when prefix is "-" or empty
                } else if prefix.starts_with("--") {
                    long_name.starts_with(&prefix) // Only show long flags matching prefix
                } else {
                    false // Don't show long flags if prefix is short flag format
                };

                if should_show_long {
                    suggestions.push(create_flag_suggestion(long_name));
                    flag_count += 1;
                }

                // Add short flag if it matches
                if let Some(short) = &short_name {
                    let flag_char = flag.short.unwrap_or(' ');
                    let should_show_short = if show_all {
                        true // Show all flags when prefix is "-" or empty
                    } else if prefix.starts_with("-") && !prefix.starts_with("--") {
                        // For combined short flags like "-a" or "-af", suggest flags that can be appended
                        // Extract already used flags from prefix (e.g., "-a" -> ['a'], "-af" -> ['a', 'f'])
                        let used_flags: Vec<char> = prefix[1..].chars().collect();

                        // Show if this flag isn't already in the prefix
                        !used_flags.contains(&flag_char)
                    } else {
                        false // Don't show short flags if prefix is long flag format
                    };

                    if should_show_short {
                        // If prefix already contains flags (like "-a"), create combined suggestion (like "-af")
                        let suggestion_name = if prefix.len() > 1 && prefix.starts_with("-") {
                            format!("{}{}", prefix, flag_char)
                        } else {
                            short.clone()
                        };
                        suggestions.push(create_flag_suggestion(suggestion_name));
                        flag_count += 1;
                    }
                }
            }
        }

        console_log!("[completion] Found {flag_count} flag suggestions");
    } else {
        console_log!("[completion] Could not find signature for command: {command_name:?}");
    }
    suggestions.sort();
    suggestions
}

pub fn generate_command_argument_suggestions(
    input: &str,
    engine_guard: &EngineState,
    working_set: &StateWorkingSet,
    prefix: String,
    span: Span,
    command_name: String,
    arg_index: usize,
    root: &std::sync::Arc<vfs::VfsPath>,
) -> Vec<Suggestion> {
    console_log!(
        "[completion] Generating CommandArgument suggestions for command: {command_name:?}, arg_index: {arg_index}, prefix: {prefix:?}"
    );

    let mut suggestions = Vec::new();
    if let Some(signature) = get_command_signature(engine_guard, &command_name) {
        // First, check if we're completing an argument for a flag
        // Look backwards from the current position to find the previous flag
        let text_before = if span.start < input.len() {
            &input[..span.start]
        } else {
            ""
        };
        let text_before_trimmed = text_before.trim_end();

        // Check if the last word before cursor is a flag
        let last_word_start = text_before_trimmed
            .rfind(|c: char| c.is_whitespace())
            .map(|i| i + 1)
            .unwrap_or(0);
        let last_word = &text_before_trimmed[last_word_start..];

        if last_word.starts_with('-') {
            // We're after a flag - check if this flag accepts an argument
            let flag_name = last_word.trim();
            let is_long_flag = flag_name.starts_with("--");
            let flag_to_match: Option<(bool, String)> = if is_long_flag {
                // Long flag: --flag-name
                flag_name.strip_prefix("--").map(|s| (true, s.to_string()))
            } else {
                // Short flag: -f (single character)
                flag_name
                    .strip_prefix("-")
                    .and_then(|s| s.chars().next().map(|c| (false, c.to_string())))
            };

            if let Some((is_long, flag_name_to_match)) = flag_to_match {
                // Find the flag in the signature
                for flag in &signature.named {
                    let matches_flag = if is_long {
                        // Long flag
                        flag.long == flag_name_to_match
                    } else {
                        // Short flag - compare character
                        flag.short
                            .map(|c| c.to_string() == flag_name_to_match)
                            .unwrap_or(false)
                    };

                    if matches_flag {
                        // Found the flag - check if it accepts an argument
                        if let Some(flag_arg_shape) = &flag.arg {
                            // Flag accepts an argument - use its type
                            console_log!(
                                "[completion] Flag {flag_name:?} accepts argument of type {:?}",
                                flag_arg_shape
                            );
                            match flag_arg_shape {
                                nu_protocol::SyntaxShape::Filepath
                                | nu_protocol::SyntaxShape::Any => {
                                    // File/directory completion for flag argument
                                    let file_suggestions = generate_file_suggestions(
                                        &prefix,
                                        span,
                                        root,
                                        Some(flag.desc.clone()),
                                        input,
                                    );
                                    let file_count = file_suggestions.len();
                                    suggestions.extend(file_suggestions);
                                    console_log!(
                                        "[completion] Found {file_count} file suggestions for flag argument"
                                    );
                                }
                                _ => {
                                    // Flag argument is not a filepath type
                                    console_log!(
                                        "[completion] Flag {flag_name:?} argument is type {:?}, not suggesting files",
                                        flag_arg_shape
                                    );
                                }
                            }
                            return suggestions;
                        } else {
                            // Flag doesn't accept an argument - fall through to positional argument check
                            console_log!(
                                "[completion] Flag {flag_name:?} doesn't accept an argument, checking positional arguments"
                            );
                            break;
                        }
                    }
                }
            }
        }

        // Not after a flag, or flag doesn't accept an argument - check positional arguments
        // Get positional arguments from signature
        // Check if argument is in required or optional positional
        let required_count = signature.required_positional.len();
        let is_optional = arg_index >= required_count;

        // Find the argument at the given index
        let arg = if arg_index < signature.required_positional.len() {
            signature.required_positional.get(arg_index)
        } else {
            let optional_index = arg_index - required_count;
            signature.optional_positional.get(optional_index)
        };

        if let Some(arg) = arg {
            // Check the SyntaxShape to determine completion type
            // Only suggest files/dirs for Filepath type
            match &arg.shape {
                nu_protocol::SyntaxShape::Filepath | nu_protocol::SyntaxShape::Any => {
                    // File/directory completion
                    let file_suggestions = generate_file_suggestions(
                        &prefix,
                        span,
                        root,
                        Some(arg.desc.clone()),
                        input,
                    );
                    let file_count = file_suggestions.len();
                    suggestions.extend(file_suggestions);
                    console_log!(
                        "[completion] Found {file_count} file suggestions for argument {arg_index}"
                    );
                }
                _ => {
                    // For other types, don't suggest files
                    console_log!(
                        "[completion] Argument {arg_index} is type {:?}, not suggesting files",
                        arg.shape
                    );
                }
            }
        } else {
            // Argument index out of range - command doesn't accept that many positional arguments
            // Don't suggest files since we know the type (it's not a valid argument)
            console_log!(
                "[completion] Argument index {arg_index} out of range, not suggesting files"
            );
        }
    } else {
        // No signature found, fall back to file completion
        console_log!(
            "[completion] Could not find signature for command: {command_name:?}, using file completion"
        );
        let file_suggestions = generate_file_suggestions(&prefix, span, root, None, input);
        suggestions.extend(file_suggestions);
    }
    suggestions.sort();
    suggestions
}

pub fn generate_variable_suggestions(
    input: &str,
    working_set: &StateWorkingSet,
    prefix: String,
    span: Span,
    byte_pos: usize,
) -> Vec<Suggestion> {
    console_log!("[completion] Generating Variable suggestions with prefix: {prefix:?}");

    // Collect all available variables
    let variables = collect_variables(working_set, input, byte_pos);
    let span = to_char_span(input, span);
    let mut suggestions = Vec::new();
    let mut var_count = 0;

    for (var_name, var_id) in variables {
        // Filter by prefix (variable name includes $, so we need to check after $)
        if var_name.len() > 1 && var_name[1..].starts_with(&prefix) {
            // Get variable type
            let var_type = working_set.get_variable(var_id).ty.to_string();

            suggestions.push(Suggestion {
                name: var_name.clone(),
                description: Some(var_type.clone()),
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

    console_log!("[completion] Found {var_count} variable suggestions");
    suggestions.sort();
    suggestions
}

pub fn generate_cell_path_suggestions(
    input: &str,
    working_set: &StateWorkingSet,
    engine_guard: &EngineState,
    stack_guard: &Stack,
    prefix: String,
    span: Span,
    var_id: nu_protocol::VarId,
    path_so_far: Vec<String>,
) -> Vec<Suggestion> {
    console_log!(
        "[completion] Generating CellPath suggestions with prefix: {prefix:?}, path: {path_so_far:?}"
    );

    let mut suggestions = Vec::new();
    // Evaluate the variable to get its value
    if let Some(var_value) =
        eval_variable_for_completion(var_id, working_set, engine_guard, stack_guard)
    {
        // Follow the path to get the value at the current level
        let current_value = if path_so_far.is_empty() {
            var_value
        } else {
            let path_refs: Vec<&str> = path_so_far.iter().map(|s| s.as_str()).collect();
            follow_cell_path(&var_value, &path_refs).unwrap_or(var_value)
        };

        // Get columns/fields from the current value
        let columns = get_columns_from_value(&current_value);
        let span = to_char_span(input, span);
        let mut field_count = 0;

        for (col_name, col_type) in columns {
            // Filter by prefix
            if col_name.starts_with(&prefix) {
                let type_str = col_type.as_deref().unwrap_or("any");
                suggestions.push(Suggestion {
                    name: col_name.clone(),
                    description: Some(type_str.to_string()),
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

        console_log!("[completion] Found {field_count} cell path suggestions");
    } else {
        // Variable couldn't be evaluated - this is expected for runtime variables
        // We can't provide cell path completions without knowing the structure
        console_log!(
            "[completion] Could not evaluate variable {var_id:?} for cell path completion (runtime variable)"
        );

        // Try to get type information to provide better feedback
        if let Ok(var_info) = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            working_set.get_variable(var_id)
        })) {
            console_log!("[completion] Variable type: {ty:?}", ty = var_info.ty);
        }
    }
    suggestions.sort();
    suggestions
}

pub fn generate_file_suggestions(
    prefix: &str,
    span: Span,
    root: &std::sync::Arc<vfs::VfsPath>,
    description: Option<String>,
    input: &str,
) -> Vec<Suggestion> {
    let (dir, file_prefix) = prefix
        .rfind('/')
        .map(|idx| (&prefix[..idx + 1], &prefix[idx + 1..]))
        .unwrap_or(("", prefix));

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

    let mut file_suggestions = Vec::new();
    if let Some(d) = target_dir {
        if let Ok(iterator) = d.read_dir() {
            let char_span = to_char_span(input, span);
            for entry in iterator {
                let name = entry.filename();
                if name.starts_with(file_prefix) {
                    let full_completion = format!("{}{}", dir, name);
                    file_suggestions.push(Suggestion {
                        name: full_completion.clone(),
                        description: description.clone(),
                        rendered: full_completion,
                        span_start: char_span.start,
                        span_end: char_span.end,
                    });
                }
            }
        }
    }
    file_suggestions
}

pub fn generate_suggestions(
    input: &str,
    contexts: Vec<CompletionContext>,
    working_set: &StateWorkingSet,
    engine_guard: &EngineState,
    stack_guard: &Stack,
    root: &std::sync::Arc<vfs::VfsPath>,
    byte_pos: usize,
) -> Vec<Suggestion> {
    console_log!("contexts: {contexts:?}");

    let mut suggestions = Vec::new();
    for context in contexts {
        let mut sug = match context {
            CompletionContext::Command {
                prefix,
                span,
                parent_command,
            } => generate_command_suggestions(input, working_set, prefix, span, parent_command),
            CompletionContext::Argument { prefix, span } => {
                generate_argument_suggestions(input, prefix, span, root)
            }
            CompletionContext::Flag {
                prefix,
                span,
                command_name,
            } => generate_flag_suggestions(input, engine_guard, prefix, span, command_name),
            CompletionContext::CommandArgument {
                prefix,
                span,
                command_name,
                arg_index,
            } => generate_command_argument_suggestions(
                input,
                engine_guard,
                working_set,
                prefix,
                span,
                command_name,
                arg_index,
                root,
            ),
            CompletionContext::Variable { prefix, span } => {
                generate_variable_suggestions(input, working_set, prefix, span, byte_pos)
            }
            CompletionContext::CellPath {
                prefix,
                span,
                var_id,
                path_so_far,
            } => generate_cell_path_suggestions(
                input,
                working_set,
                engine_guard,
                stack_guard,
                prefix,
                span,
                var_id,
                path_so_far,
            ),
        };
        suggestions.append(&mut sug);
    }

    suggestions
}
