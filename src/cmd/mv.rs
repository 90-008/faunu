use std::io::{Read, Write};

use crate::{
    cmd::glob::{GlobOptions, expand_path},
    error::to_shell_err,
    globals::{get_pwd, get_vfs},
};
use nu_engine::CallExt;
use nu_protocol::{
    Category, PipelineData, ShellError, Signature, SyntaxShape, Type, Value,
    engine::{Command, EngineState, Stack},
};
use std::sync::Arc;
use vfs::{VfsError, VfsFileType};

#[derive(Clone)]
pub struct Mv;

impl Command for Mv {
    fn name(&self) -> &str {
        "mv"
    }

    fn signature(&self) -> Signature {
        Signature::build("mv")
            .required(
                "source",
                SyntaxShape::OneOf(vec![SyntaxShape::Filepath, SyntaxShape::GlobPattern]),
                "path to the file or directory to move",
            )
            .required(
                "destination",
                SyntaxShape::Filepath,
                "path to the destination",
            )
            .input_output_type(Type::Nothing, Type::Nothing)
            .category(Category::FileSystem)
    }

    fn description(&self) -> &str {
        "move a file or directory in the virtual filesystem."
    }

    fn run(
        &self,
        engine_state: &EngineState,
        stack: &mut Stack,
        call: &nu_protocol::engine::Call,
        _input: PipelineData,
    ) -> Result<PipelineData, ShellError> {
        let source_value: Value = call.req(engine_state, stack, 0)?;
        let dest_path: String = call.req(engine_state, stack, 1)?;

        let source_str = match source_value {
            Value::String { val, .. } | Value::Glob { val, .. } => val,
            _ => {
                return Err(ShellError::GenericError {
                    error: "invalid source path".into(),
                    msg: "source must be a string or glob pattern".into(),
                    span: Some(call.arguments_span()),
                    help: None,
                    inner: vec![],
                });
            }
        };

        // Prevent moving root
        if source_str == "/" {
            return Err(ShellError::GenericError {
                error: "cannot move root".to_string(),
                msg: "refusing to move root directory".to_string(),
                span: Some(call.arguments_span()),
                help: None,
                inner: vec![],
            });
        }

        // Expand source path (glob or single) into list of paths
        let is_absolute = source_str.starts_with('/');
        let base_path: Arc<vfs::VfsPath> = if is_absolute { get_vfs() } else { get_pwd() };

        let options = GlobOptions {
            max_depth: None,
            no_dirs: false,
            no_files: false,
        };

        let matches = expand_path(&source_str, base_path.clone(), options)?;
        let is_glob = matches.len() > 1
            || source_str.contains('*')
            || source_str.contains('?')
            || source_str.contains('[')
            || source_str.contains("**");

        // Resolve destination
        let dest = get_pwd()
            .join(dest_path.trim_end_matches('/'))
            .map_err(to_shell_err(call.arguments_span()))?;

        // For glob patterns, destination must be a directory
        if is_glob {
            let dest_meta = dest
                .metadata()
                .map_err(to_shell_err(call.arguments_span()))?;
            if dest_meta.file_type != VfsFileType::Directory {
                return Err(ShellError::GenericError {
                    error: "destination must be a directory".to_string(),
                    msg: "when using glob patterns, destination must be a directory".to_string(),
                    span: Some(call.arguments_span()),
                    help: None,
                    inner: vec![],
                });
            }
        }

        // Move each matching file/directory
        for rel_path in matches {
            let source = base_path
                .join(&rel_path)
                .map_err(to_shell_err(call.arguments_span()))?;
            let source_meta = source
                .metadata()
                .map_err(to_shell_err(call.arguments_span()))?;

            // Determine destination path
            let dest_entry = if is_glob {
                // For glob patterns, use filename in destination directory
                let filename = rel_path.split('/').last().unwrap_or(&rel_path);
                dest.join(filename)
                    .map_err(to_shell_err(call.arguments_span()))?
            } else {
                // For single path, use destination as-is
                dest.clone()
            };

            match source_meta.file_type {
                VfsFileType::File => move_file(&source, &dest_entry, call.arguments_span())?,
                VfsFileType::Directory => {
                    move_directory(&source, &dest_entry, call.arguments_span())?
                }
            }
        }

        Ok(PipelineData::Empty)
    }
}

fn move_file(
    source: &vfs::VfsPath,
    dest: &vfs::VfsPath,
    span: nu_protocol::Span,
) -> Result<(), ShellError> {
    // Read source file content
    let mut source_file = source.open_file().map_err(to_shell_err(span))?;

    let mut contents = Vec::new();
    source_file
        .read_to_end(&mut contents)
        .map_err(|e| ShellError::GenericError {
            error: "io error".to_string(),
            msg: format!("failed to read source file: {}", e),
            span: Some(span),
            help: None,
            inner: vec![],
        })?;

    // Create destination file and write content
    dest.create_file()
        .map_err(to_shell_err(span))
        .and_then(|mut f| {
            f.write_all(&contents)
                .map_err(VfsError::from)
                .map_err(to_shell_err(span))
        })?;

    // Remove source file
    source.remove_file().map_err(to_shell_err(span))?;

    Ok(())
}

fn move_directory(
    source: &vfs::VfsPath,
    dest: &vfs::VfsPath,
    span: nu_protocol::Span,
) -> Result<(), ShellError> {
    // Try to create destination directory (create_dir_all handles parent creation)
    // If it already exists, that's fine - we'll move entries into it
    let _ = dest.create_dir_all().map_err(to_shell_err(span));

    // Recursively move all entries
    let entries = source.read_dir().map_err(to_shell_err(span))?;
    for entry_name in entries {
        let source_entry = source
            .join(entry_name.as_str())
            .map_err(to_shell_err(span))?;
        let dest_entry = dest.join(entry_name.as_str()).map_err(to_shell_err(span))?;

        let entry_meta = source_entry.metadata().map_err(to_shell_err(span))?;
        match entry_meta.file_type {
            VfsFileType::File => move_file(&source_entry, &dest_entry, span)?,
            VfsFileType::Directory => move_directory(&source_entry, &dest_entry, span)?,
        }
    }

    // Remove source directory
    source.remove_dir_all().map_err(to_shell_err(span))?;

    Ok(())
}
