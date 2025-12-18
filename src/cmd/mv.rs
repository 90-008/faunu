use std::io::{Read, Write};

use crate::globals::{get_pwd, to_shell_err};
use nu_engine::CallExt;
use nu_protocol::{
    Category, PipelineData, ShellError, Signature, SyntaxShape, Type,
    engine::{Command, EngineState, Stack},
};
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
                SyntaxShape::Filepath,
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
        let source_path: String = call.req(engine_state, stack, 0)?;
        let dest_path: String = call.req(engine_state, stack, 1)?;

        // Prevent moving root
        if source_path == "/" {
            return Err(ShellError::GenericError {
                error: "cannot move root".to_string(),
                msg: "refusing to move root directory".to_string(),
                span: Some(call.arguments_span()),
                help: None,
                inner: vec![],
            });
        }

        // Resolve source relative to PWD (or absolute if path starts with '/')
        let source = get_pwd()
            .join(source_path.trim_end_matches('/'))
            .map_err(to_shell_err(call.arguments_span()))?;

        // Resolve destination relative to PWD (or absolute if path starts with '/')
        let dest = get_pwd()
            .join(dest_path.trim_end_matches('/'))
            .map_err(to_shell_err(call.arguments_span()))?;

        // Check that source exists
        let meta = source
            .metadata()
            .map_err(to_shell_err(call.arguments_span()))?;

        match meta.file_type {
            VfsFileType::File => move_file(&source, &dest, call.arguments_span())?,
            VfsFileType::Directory => move_directory(&source, &dest, call.arguments_span())?,
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
