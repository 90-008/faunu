use std::time::{SystemTime, UNIX_EPOCH};

use crate::{
    cmd::glob::{expand_path, GlobOptions},
    error::to_shell_err,
    globals::{get_pwd, get_vfs},
};
use std::sync::Arc;
use jacquard::chrono;
use nu_engine::CallExt;
use nu_protocol::{
    Category, ListStream, PipelineData, Record, ShellError, Signature, SyntaxShape, Type, Value,
    engine::{Command, EngineState, Stack},
};

#[derive(Clone)]
pub struct Ls;

impl Command for Ls {
    fn name(&self) -> &str {
        "ls"
    }

    fn signature(&self) -> Signature {
        Signature::build("ls")
            .optional(
                "path",
                SyntaxShape::OneOf(vec![SyntaxShape::Filepath, SyntaxShape::GlobPattern]),
                "the path to list",
            )
            .switch(
                "all",
                "include hidden paths (that start with a dot)",
                Some('a'),
            )
            .switch(
                "long",
                "show detailed information about each file",
                Some('l'),
            )
            .switch("full-paths", "display paths as absolute paths", Some('f'))
            .input_output_type(Type::Nothing, Type::record())
            .category(Category::FileSystem)
    }

    fn description(&self) -> &str {
        "list the files in the virtual filesystem."
    }

    fn run(
        &self,
        engine_state: &EngineState,
        stack: &mut Stack,
        call: &nu_protocol::engine::Call,
        _input: PipelineData,
    ) -> Result<PipelineData, ShellError> {
        let path_arg: Option<Value> = call.opt(engine_state, stack, 0)?;
        let all = call.has_flag(engine_state, stack, "all")?;
        let long = call.has_flag(engine_state, stack, "long")?;
        let full_paths = call.has_flag(engine_state, stack, "full-paths")?;

        let pwd = get_pwd();
        let span = call.head;

        // If no path provided, list current directory
        let (matches, base_path) = if let Some(path_val) = &path_arg {
            let path_str = match path_val {
                Value::String { val, .. } | Value::Glob { val, .. } => val,
                _ => {
                    return Err(ShellError::GenericError {
                        error: "invalid path".into(),
                        msg: "path must be a string or glob pattern".into(),
                        span: Some(call.arguments_span()),
                        help: None,
                        inner: vec![],
                    });
                }
            };

            let is_absolute = path_str.starts_with('/');
            let base_path: Arc<vfs::VfsPath> = if is_absolute {
                get_vfs()
            } else {
                pwd.clone()
            };

            let options = GlobOptions {
                max_depth: None,
                no_dirs: false,
                no_files: false,
            };

            let matches = expand_path(path_str, base_path.clone(), options)?;
            (matches, base_path)
        } else {
            // No path: list current directory entries
            let entries = pwd.read_dir().map_err(to_shell_err(span))?;
            let matches: Vec<String> = entries.map(|e| e.filename()).collect();
            (matches, pwd.clone())
        };

        let make_record = move |rel_path: &str| {
            let full_path = base_path.join(rel_path).map_err(to_shell_err(span))?;
            let metadata = full_path.metadata().map_err(to_shell_err(span))?;

            // Filter hidden files if --all is not set
            let filename = rel_path.split('/').last().unwrap_or(rel_path);
            if filename.starts_with('.') && !all {
                return Ok(None);
            }

            let type_str = match metadata.file_type {
                vfs::VfsFileType::Directory => "dir",
                vfs::VfsFileType::File => "file",
            };

            let mut record = Record::new();
            let display_name = if full_paths {
                full_path.as_str().to_string()
            } else {
                rel_path.to_string()
            };
            record.push("name", Value::string(display_name, span));
            record.push("type", Value::string(type_str, span));
            record.push("size", Value::filesize(metadata.len as i64, span));
            let mut add_timestamp = |field: &str, timestamp: Option<SystemTime>| {
                if let Some(dur) = timestamp.and_then(|s| s.duration_since(UNIX_EPOCH).ok()) {
                    record.push(
                        field,
                        Value::date(
                            chrono::DateTime::from_timestamp_nanos(dur.as_nanos() as i64)
                                .fixed_offset(),
                            span,
                        ),
                    );
                } else {
                    record.push(field, Value::nothing(span));
                }
            };
            add_timestamp("modified", metadata.modified);
            if long {
                add_timestamp("created", metadata.created);
                add_timestamp("accessed", metadata.accessed);
            }
            Ok(Some(Value::record(record, span)))
        };

        let entries = matches
            .into_iter()
            .flat_map(move |rel_path| {
                make_record(&rel_path)
                    .transpose()
                    .map(|res| res.unwrap_or_else(|err| Value::error(err, span)))
            });

        let signals = engine_state.signals().clone();
        Ok(PipelineData::list_stream(
            ListStream::new(entries, span, signals.clone()),
            Some(nu_protocol::PipelineMetadata {
                data_source: nu_protocol::DataSource::Ls,
                content_type: None,
                custom: Record::new(),
            }),
        ))
    }
}
