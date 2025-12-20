use crate::{
    cmd::glob::glob_match,
    error::{CommandError, to_shell_err},
    globals::{get_pwd, get_vfs, print_to_console, set_pwd},
};
use nu_engine::{CallExt, get_eval_block_with_early_return};
use nu_parser::parse;
use nu_protocol::{
    Category, PipelineData, ShellError, Signature, SyntaxShape, Type, Value,
    engine::{Command, EngineState, Stack, StateWorkingSet},
};
use std::sync::Arc;

#[derive(Clone)]
pub struct SourceFile;

impl Command for SourceFile {
    fn name(&self) -> &str {
        "eval file"
    }

    fn signature(&self) -> Signature {
        Signature::build(self.name())
            .required(
                "path",
                SyntaxShape::OneOf(vec![SyntaxShape::Filepath, SyntaxShape::GlobPattern]),
                "the file to source",
            )
            .input_output_type(Type::Nothing, Type::Nothing)
            .category(Category::Core)
    }

    fn description(&self) -> &str {
        "sources a file from the virtual filesystem."
    }

    fn run(
        &self,
        engine_state: &EngineState,
        stack: &mut Stack,
        call: &nu_protocol::engine::Call,
        _input: PipelineData,
    ) -> Result<PipelineData, ShellError> {
        let span = call.arguments_span();
        let path: Value = call.req(engine_state, stack, 0)?;

        // Check if path is a glob pattern
        let path_str = match &path {
            Value::String { val, .. } | Value::Glob { val, .. } => val.clone(),
            _ => {
                return Err(ShellError::GenericError {
                    error: "not a path or glob pattern".into(),
                    msg: String::new(),
                    span: Some(span),
                    help: None,
                    inner: vec![],
                });
            }
        };

        let pwd = get_pwd();
        let is_absolute = path_str.starts_with('/');
        let base_path: Arc<vfs::VfsPath> = if is_absolute { get_vfs() } else { pwd.clone() };

        // Check if it's a glob pattern (contains *, ?, [, or **)
        let is_glob = path_str.contains('*')
            || path_str.contains('?')
            || path_str.contains('[')
            || path_str.contains("**");

        let paths_to_source = if is_glob {
            // Expand glob pattern
            let options = crate::cmd::glob::GlobOptions {
                max_depth: None,
                no_dirs: true, // Only source files, not directories
                no_files: false,
            };
            glob_match(&path_str, base_path.clone(), options)?
        } else {
            // Single file path
            vec![path_str]
        };

        // Source each matching file
        for rel_path in paths_to_source {
            let full_path = base_path.join(&rel_path).map_err(to_shell_err(span))?;

            let metadata = full_path.metadata().map_err(to_shell_err(span))?;
            if metadata.file_type != vfs::VfsFileType::File {
                continue;
            }

            let contents = full_path.read_to_string().map_err(to_shell_err(span))?;

            set_pwd(full_path.parent().into());
            let res = eval(engine_state, stack, &contents, Some(&full_path.filename()));
            set_pwd(pwd.clone());

            match res {
                Ok(p) => {
                    print_to_console(&p.collect_string("\n", &engine_state.config)?, true);
                }
                Err(err) => {
                    let msg: String = err.into();
                    print_to_console(&msg, true);
                    return Err(ShellError::GenericError {
                        error: "source error".into(),
                        msg: format!("can't source file: {}", rel_path),
                        span: Some(span),
                        help: None,
                        inner: vec![],
                    });
                }
            }
        }

        Ok(PipelineData::Empty)
    }
}

pub fn eval(
    engine_state: &EngineState,
    stack: &mut Stack,
    contents: &str,
    filename: Option<&str>,
) -> Result<PipelineData, CommandError> {
    let filename = filename.unwrap_or("<piped data>");
    let mut working_set = StateWorkingSet::new(engine_state);
    let start_offset = working_set.next_span_start();
    let _ = working_set.add_file(filename.into(), contents.as_bytes());

    let block = parse(&mut working_set, Some(filename), contents.as_bytes(), false);

    if let Some(err) = working_set.parse_errors.into_iter().next() {
        web_sys::console::error_1(&err.to_string().into());
        return Err(CommandError::new(err, contents).with_start_offset(start_offset));
    }
    if let Some(err) = working_set.compile_errors.into_iter().next() {
        web_sys::console::error_1(&err.to_string().into());
        return Err(CommandError::new(err, contents).with_start_offset(start_offset));
    }

    // uhhhhh this is safe prolly cuz we are single threaded
    // i mean still shouldnt do this but i lowkey dont care so :3
    let engine_state = unsafe {
        std::ptr::from_ref(engine_state)
            .cast_mut()
            .as_mut()
            .unwrap()
    };
    engine_state
        .merge_delta(working_set.delta)
        .map_err(|err| CommandError::new(err, contents).with_start_offset(start_offset))?;

    // queue_delta(working_set.delta.clone());

    let eval_block_with_early_return = get_eval_block_with_early_return(&engine_state);
    eval_block_with_early_return(&engine_state, stack, &block, PipelineData::Empty)
        .map(|d| d.body)
        .map_err(|err| CommandError::new(err, contents).with_start_offset(start_offset))
}
