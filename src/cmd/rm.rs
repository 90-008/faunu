use crate::{
    cmd::glob::{expand_path, GlobOptions},
    error::to_shell_err,
    globals::{get_pwd, get_vfs},
};
use std::sync::Arc;
use nu_engine::CallExt;
use nu_protocol::{
    Category, PipelineData, ShellError, Signature, SyntaxShape, Type, Value,
    engine::{Command, EngineState, Stack},
};
use vfs::VfsFileType;

#[derive(Clone)]
pub struct Rm;

impl Command for Rm {
    fn name(&self) -> &str {
        "rm"
    }

    fn signature(&self) -> Signature {
        Signature::build("rm")
            .required(
                "path",
                SyntaxShape::OneOf(vec![SyntaxShape::String, SyntaxShape::GlobPattern]),
                "path to file or directory to remove",
            )
            .switch(
                "recursive",
                "remove directories and their contents recursively",
                Some('r'),
            )
            .input_output_type(Type::Nothing, Type::Nothing)
            .category(Category::FileSystem)
    }

    fn description(&self) -> &str {
        "remove a file or directory from the virtual filesystem."
    }

    fn run(
        &self,
        engine_state: &EngineState,
        stack: &mut Stack,
        call: &nu_protocol::engine::Call,
        _input: PipelineData,
    ) -> Result<PipelineData, ShellError> {
        let path_value: Value = call.req(engine_state, stack, 0)?;
        let recursive = call.has_flag(engine_state, stack, "recursive")?;

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

        // Prevent removing root
        if path_str == "/" {
            return Err(ShellError::GenericError {
                error: "cannot remove root".to_string(),
                msg: "refusing to remove root directory".to_string(),
                span: Some(call.head),
                help: None,
                inner: vec![],
            });
        }

        // Expand path (glob or single) into list of paths
        let is_absolute = path_str.starts_with('/');
        let base_path: Arc<vfs::VfsPath> = if is_absolute {
            get_vfs()
        } else {
            get_pwd()
        };

        let options = GlobOptions {
            max_depth: None,
            no_dirs: false,
            no_files: false,
        };

        let matches = expand_path(&path_str, base_path.clone(), options)?;

        // Remove all matching paths
        for rel_path in matches {
            let target = base_path.join(&rel_path).map_err(to_shell_err(call.head))?;
            let meta = target.metadata().map_err(to_shell_err(call.head))?;
            match meta.file_type {
                VfsFileType::File => {
                    target.remove_file().map_err(to_shell_err(call.head))?;
                }
                VfsFileType::Directory => {
                    (if recursive {
                        target.remove_dir_all()
                    } else {
                        target.remove_dir()
                    })
                    .map_err(to_shell_err(call.head))?;
                }
            }
        }

        Ok(PipelineData::Empty)
    }
}
