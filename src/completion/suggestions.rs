use crate::completion::context::get_command_signature;
use crate::completion::files::generate_file_suggestions;
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
) -> Vec<Suggestion> {
    console_log!("[completion] Generating Command suggestions with prefix: {prefix:?}");
    // Command completion
    let cmds =
        working_set.find_commands_by_predicate(|value| value.starts_with(prefix.as_bytes()), true);

    let span = to_char_span(input, span);
    let mut suggestions = Vec::new();
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
    console_log!("[completion] Found {cmd_count} command suggestions");
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
    let file_suggestions = generate_file_suggestions(&prefix, span, root, None, input);
    let file_count = file_suggestions.len();
    console_log!("[completion] Found {file_count} file suggestions");
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
                        is_command: false,
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
                    let should_show_short = if show_all {
                        true // Show all flags when prefix is "-" or empty
                    } else if prefix.starts_with("-") && !prefix.starts_with("--") {
                        short.starts_with(&prefix) // Only show short flags matching prefix
                    } else {
                        false // Don't show short flags if prefix is long flag format
                    };

                    if should_show_short {
                        suggestions.push(create_flag_suggestion(short.clone()));
                        flag_count += 1;
                    }
                }
            }
        }

        console_log!("[completion] Found {flag_count} flag suggestions");
    } else {
        console_log!("[completion] Could not find signature for command: {command_name:?}");
    }
    suggestions
}

pub fn generate_command_argument_suggestions(
    input: &str,
    engine_guard: &EngineState,
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
        // Get positional arguments from signature
        // Combine required and optional positional arguments
        let mut all_positional = Vec::new();
        all_positional.extend_from_slice(&signature.required_positional);
        all_positional.extend_from_slice(&signature.optional_positional);

        // Find the argument at the given index
        if let Some(arg) = all_positional.get(arg_index) {
            // Check the SyntaxShape to determine completion type
            // Only suggest files/dirs for Filepath type (or "any" when type is unknown)
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
            // Argument index out of range, fall back to file completion
            console_log!(
                "[completion] Argument index {arg_index} out of range, using file completion"
            );
            // Use the same file completion logic as Argument context
            let file_suggestions = generate_file_suggestions(&prefix, span, root, None, input);
            suggestions.extend(file_suggestions);
        }
    } else {
        // No signature found, fall back to file completion
        console_log!(
            "[completion] Could not find signature for command: {command_name:?}, using file completion"
        );
        let file_suggestions = generate_file_suggestions(&prefix, span, root, None, input);
        suggestions.extend(file_suggestions);
    }
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

    console_log!("[completion] Found {var_count} variable suggestions");
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
    suggestions
}

pub fn generate_suggestions(
    input: &str,
    context: Option<CompletionContext>,
    working_set: &StateWorkingSet,
    engine_guard: &EngineState,
    stack_guard: &Stack,
    root: &std::sync::Arc<vfs::VfsPath>,
    byte_pos: usize,
) -> Vec<Suggestion> {
    console_log!("context: {context:?}");

    match context {
        Some(CompletionContext::Command { prefix, span }) => {
            generate_command_suggestions(input, working_set, prefix, span)
        }
        Some(CompletionContext::Argument { prefix, span }) => {
            generate_argument_suggestions(input, prefix, span, root)
        }
        Some(CompletionContext::Flag {
            prefix,
            span,
            command_name,
        }) => generate_flag_suggestions(input, engine_guard, prefix, span, command_name),
        Some(CompletionContext::CommandArgument {
            prefix,
            span,
            command_name,
            arg_index,
        }) => generate_command_argument_suggestions(
            input,
            engine_guard,
            prefix,
            span,
            command_name,
            arg_index,
            root,
        ),
        Some(CompletionContext::Variable { prefix, span }) => {
            generate_variable_suggestions(input, working_set, prefix, span, byte_pos)
        }
        Some(CompletionContext::CellPath {
            prefix,
            span,
            var_id,
            path_so_far,
        }) => generate_cell_path_suggestions(
            input,
            working_set,
            engine_guard,
            stack_guard,
            prefix,
            span,
            var_id,
            path_so_far,
        ),
        _ => {
            console_log!("[completion] Context is None, no suggestions generated");
            Vec::new()
        }
    }
}
