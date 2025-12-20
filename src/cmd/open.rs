use std::ops::Not;

use crate::{
    cmd::glob::{GlobOptions, expand_path},
    globals::{get_pwd, get_vfs},
};
use nu_command::{FromCsv, FromJson, FromOds, FromToml, FromTsv, FromXlsx, FromXml, FromYaml};
use nu_engine::CallExt;
use nu_protocol::{
    ByteStream, Category, ListStream, PipelineData, ShellError, Signature, SyntaxShape, Type,
    Value,
    engine::{Command, EngineState, Stack},
};
use std::sync::Arc;

#[derive(Clone)]
pub struct Open;

impl Command for Open {
    fn name(&self) -> &str {
        "open"
    }

    fn signature(&self) -> Signature {
        Signature::build("open")
            .required(
                "path",
                SyntaxShape::OneOf(vec![SyntaxShape::Filepath, SyntaxShape::GlobPattern]),
                "path to the file",
            )
            .switch(
                "raw",
                "output content as raw string/binary without parsing",
                Some('r'),
            )
            .input_output_type(Type::Nothing, Type::one_of([Type::String, Type::Binary]))
            .category(Category::FileSystem)
    }

    fn description(&self) -> &str {
        "open a file from the virtual filesystem."
    }

    fn run(
        &self,
        engine_state: &EngineState,
        stack: &mut Stack,
        call: &nu_protocol::engine::Call,
        _input: PipelineData,
    ) -> Result<PipelineData, ShellError> {
        let path_value: Value = call.req(engine_state, stack, 0)?;
        let raw_flag = call.has_flag(engine_state, stack, "raw")?;

        let path_str = match path_value {
            Value::String { val, .. } | Value::Glob { val, .. } => val,
            _ => {
                return Err(ShellError::GenericError {
                    error: "invalid path".into(),
                    msg: "path must be a string or glob pattern".into(),
                    span: Some(call.head),
                    help: None,
                    inner: vec![],
                });
            }
        };

        // Expand path (glob or single) into list of paths
        let is_absolute = path_str.starts_with('/');
        let base_path: Arc<vfs::VfsPath> = if is_absolute { get_vfs() } else { get_pwd() };

        let options = GlobOptions {
            max_depth: None,
            no_dirs: true, // Only open files, not directories
            no_files: false,
        };

        let matches = expand_path(&path_str, base_path.clone(), options)?;

        let span = call.head;
        let signals = engine_state.signals().clone();

        // Open each matching file
        let mut results = Vec::new();
        for rel_path in matches {
            let target_file = match base_path.join(&rel_path) {
                Ok(p) => p,
                Err(e) => {
                    results.push(Value::error(
                        ShellError::GenericError {
                            error: "path error".into(),
                            msg: e.to_string(),
                            span: Some(span),
                            help: None,
                            inner: vec![],
                        },
                        span,
                    ));
                    continue;
                }
            };

            let parse_cmd = raw_flag
                .not()
                .then(|| {
                    target_file
                        .extension()
                        .and_then(|ext| get_cmd_for_ext(&ext))
                })
                .flatten();

            match target_file.open_file() {
                Ok(f) => {
                    let data = PipelineData::ByteStream(
                        ByteStream::read(
                            f,
                            span,
                            signals.clone(),
                            nu_protocol::ByteStreamType::String,
                        ),
                        None,
                    );

                    let value = if let Some(cmd) = parse_cmd {
                        match cmd.run(engine_state, stack, call, data) {
                            Ok(pipeline_data) => {
                                // Convert pipeline data to value
                                pipeline_data
                                    .into_value(span)
                                    .unwrap_or_else(|e| Value::error(e, span))
                            }
                            Err(e) => Value::error(e, span),
                        }
                    } else {
                        data.into_value(span)
                            .unwrap_or_else(|e| Value::error(e, span))
                    };
                    results.push(value);
                }
                Err(e) => {
                    results.push(Value::error(
                        ShellError::GenericError {
                            error: "io error".into(),
                            msg: format!("failed to open file {}: {}", rel_path, e),
                            span: Some(span),
                            help: None,
                            inner: vec![],
                        },
                        span,
                    ));
                }
            }
        }

        // If single file, return the single result directly (for backward compatibility)
        if results.len() == 1
            && !path_str.contains('*')
            && !path_str.contains('?')
            && !path_str.contains('[')
            && !path_str.contains("**")
        {
            match results.into_iter().next().unwrap() {
                Value::Error { error, .. } => Err(*error),
                val => Ok(PipelineData::Value(val, None)),
            }
        } else {
            Ok(PipelineData::list_stream(
                ListStream::new(results.into_iter(), span, signals.clone()),
                None,
            ))
        }
    }
}

fn get_cmd_for_ext(ext: &str) -> Option<Box<dyn Command>> {
    Some(match ext {
        "json" => Box::new(FromJson),
        "yaml" | "yml" => Box::new(FromYaml),
        "toml" => Box::new(FromToml),
        "csv" => Box::new(FromCsv),
        "ods" => Box::new(FromOds),
        "tsv" => Box::new(FromTsv),
        "xml" => Box::new(FromXml),
        "xlsx" => Box::new(FromXlsx),
        _ => return None,
    })
}
