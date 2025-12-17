use crate::console_log;
use nu_protocol::engine::{EngineState, Stack, StateWorkingSet};
use nu_protocol::{ENV_VARIABLE_ID, IN_VARIABLE_ID, NU_VARIABLE_ID, Span, Value};
use std::collections::HashMap;

pub fn eval_variable_for_completion(
    var_id: nu_protocol::VarId,
    working_set: &StateWorkingSet,
    engine_guard: &EngineState,
    stack_guard: &Stack,
) -> Option<Value> {
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
                        console_log!("[completion] Found variable {var_id:?} value in stack");
                        Some(value)
                    }
                    Err(_) => {
                        // Variable not in stack either
                        console_log!(
                            "[completion] Variable {var_id:?} has no const value and not in stack, type: {ty:?}",
                            ty = var_info.ty
                        );
                        None
                    }
                }
            }
        }
    }
}

pub fn get_columns_from_value(value: &Value) -> Vec<(String, Option<String>)> {
    match value {
        Value::Record { val, .. } => val
            .iter()
            .map(|(name, v)| (name.to_string(), Some(v.get_type().to_string())))
            .collect(),
        Value::List { vals, .. } => {
            // Get common columns from list of records
            if let Some(first) = vals.first() {
                if let Value::Record { val, .. } = first {
                    return val
                        .iter()
                        .map(|(name, v)| (name.to_string(), Some(v.get_type().to_string())))
                        .collect();
                }
            }
            vec![]
        }
        _ => vec![],
    }
}

pub fn follow_cell_path(value: &Value, path: &[&str]) -> Option<Value> {
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
}

pub fn extract_closure_params(input: &str, cursor_pos: usize) -> Vec<String> {
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
            console_log!(
                "[completion] Found closure at [{start}, {end}) containing cursor {cursor_pos}, params: {param_names:?}"
            );
            params.extend(param_names);
        }
    }

    params
}

pub fn collect_variables(
    working_set: &StateWorkingSet,
    input: &str,
    cursor_pos: usize,
) -> HashMap<String, nu_protocol::VarId> {
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
        console_log!("[completion] Added closure parameter: {var_name:?}");
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
}
