use std::sync::Arc;

use crate::globals::{get_pwd, get_vfs};
use nu_engine::CallExt;
use nu_glob::Pattern;
use nu_protocol::{
    Category, ListStream, PipelineData, ShellError, Signature, SyntaxShape, Type, Value,
    engine::{Command, EngineState, Stack},
};
use vfs::VfsFileType;

/// Options for glob matching
pub struct GlobOptions {
    pub max_depth: Option<usize>,
    pub no_dirs: bool,
    pub no_files: bool,
}

impl Default for GlobOptions {
    fn default() -> Self {
        Self {
            max_depth: None,
            no_dirs: false,
            no_files: false,
        }
    }
}

/// Expand a path (glob pattern or regular path) into a list of matching paths.
/// If the path is not a glob pattern, returns a single-item list.
/// Returns a vector of relative paths (relative to the base path).
pub fn expand_path(
    path_str: &str,
    base_path: Arc<vfs::VfsPath>,
    options: GlobOptions,
) -> Result<Vec<String>, ShellError> {
    // Check if it's a glob pattern
    let is_glob = path_str.contains('*')
        || path_str.contains('?')
        || path_str.contains('[')
        || path_str.contains("**");

    if is_glob {
        glob_match(path_str, base_path, options)
    } else {
        // Single path: return as single-item list
        Ok(vec![path_str.trim_start_matches('/').to_string()])
    }
}

/// Match files and directories using a glob pattern.
/// Returns a vector of relative paths (relative to the base path) that match the pattern.
pub fn glob_match(
    pattern_str: &str,
    base_path: Arc<vfs::VfsPath>,
    options: GlobOptions,
) -> Result<Vec<String>, ShellError> {
    if pattern_str.is_empty() {
        return Err(ShellError::GenericError {
            error: "glob pattern must not be empty".into(),
            msg: "glob pattern is empty".into(),
            span: None,
            help: Some("add characters to the glob pattern".into()),
            inner: vec![],
        });
    }

    // Parse the pattern
    let pattern = Pattern::new(pattern_str).map_err(|e| ShellError::GenericError {
        error: "error with glob pattern".into(),
        msg: format!("{}", e),
        span: None,
        help: None,
        inner: vec![],
    })?;

    // Determine max depth
    let max_depth = if let Some(d) = options.max_depth {
        d
    } else if pattern_str.contains("**") {
        usize::MAX
    } else {
        // Count number of / in pattern to determine depth
        pattern_str.split('/').count()
    };

    // Normalize pattern: remove leading / for relative matching
    let normalized_pattern = pattern_str.trim_start_matches('/');
    let is_recursive = normalized_pattern.contains("**");

    // Collect matching paths
    let mut matches = Vec::new();

    fn walk_directory(
        current_path: Arc<vfs::VfsPath>,
        current_relative_path: String,
        pattern: &Pattern,
        normalized_pattern: &str,
        current_depth: usize,
        max_depth: usize,
        matches: &mut Vec<String>,
        no_dirs: bool,
        no_files: bool,
        is_recursive: bool,
    ) -> Result<(), ShellError> {
        if current_depth > max_depth {
            return Ok(());
        }

        // Walk through directory entries
        if let Ok(entries) = current_path.read_dir() {
            for entry in entries {
                let filename = entry.filename();
                let entry_path =
                    current_path
                        .join(&filename)
                        .map_err(|e| ShellError::GenericError {
                            error: "path error".into(),
                            msg: e.to_string(),
                            span: None,
                            help: None,
                            inner: vec![],
                        })?;

                // Build relative path from base
                let new_relative = if current_relative_path.is_empty() {
                    filename.clone()
                } else {
                    format!("{}/{}", current_relative_path, filename)
                };

                let metadata = entry_path
                    .metadata()
                    .map_err(|e| ShellError::GenericError {
                        error: "path error".into(),
                        msg: e.to_string(),
                        span: None,
                        help: None,
                        inner: vec![],
                    })?;

                // Check if this path matches the pattern
                // For patterns without path separators, match just the filename
                // For patterns with path separators, match the full relative path
                let path_to_match = if normalized_pattern.contains('/') {
                    &new_relative
                } else {
                    &filename
                };

                if pattern.matches(path_to_match) {
                    let should_include = match metadata.file_type {
                        VfsFileType::Directory => !no_dirs,
                        VfsFileType::File => !no_files,
                    };
                    if should_include {
                        matches.push(new_relative.clone());
                    }
                }

                // Recursively walk into subdirectories
                if metadata.file_type == VfsFileType::Directory {
                    // Continue if: recursive pattern, or we haven't reached max depth, or pattern has more components
                    let should_recurse = is_recursive
                        || current_depth < max_depth
                        || (normalized_pattern.contains('/')
                            && current_depth < normalized_pattern.split('/').count());

                    if should_recurse {
                        walk_directory(
                            Arc::new(entry_path),
                            new_relative,
                            pattern,
                            normalized_pattern,
                            current_depth + 1,
                            max_depth,
                            matches,
                            no_dirs,
                            no_files,
                            is_recursive,
                        )?;
                    }
                }
            }
        }

        Ok(())
    }

    // Start walking from base path
    walk_directory(
        base_path,
        String::new(),
        &pattern,
        normalized_pattern,
        0,
        max_depth,
        &mut matches,
        options.no_dirs,
        options.no_files,
        is_recursive,
    )?;

    Ok(matches)
}

#[derive(Clone)]
pub struct Glob;

impl Command for Glob {
    fn name(&self) -> &str {
        "glob"
    }

    fn signature(&self) -> Signature {
        Signature::build("glob")
            .required(
                "pattern",
                SyntaxShape::OneOf(vec![SyntaxShape::String, SyntaxShape::GlobPattern]),
                "The glob expression.",
            )
            .named(
                "depth",
                SyntaxShape::Int,
                "directory depth to search",
                Some('d'),
            )
            .switch(
                "no-dir",
                "Whether to filter out directories from the returned paths",
                Some('D'),
            )
            .switch(
                "no-file",
                "Whether to filter out files from the returned paths",
                Some('F'),
            )
            .input_output_type(Type::Nothing, Type::List(Box::new(Type::String)))
            .category(Category::FileSystem)
    }

    fn description(&self) -> &str {
        "Creates a list of files and/or folders based on the glob pattern provided."
    }

    fn run(
        &self,
        engine_state: &EngineState,
        stack: &mut Stack,
        call: &nu_protocol::engine::Call,
        _input: PipelineData,
    ) -> Result<PipelineData, ShellError> {
        let span = call.head;
        let pattern_value: Value = call.req(engine_state, stack, 0)?;
        let pattern_span = pattern_value.span();
        let depth: Option<i64> = call.get_flag(engine_state, stack, "depth")?;
        let no_dirs = call.has_flag(engine_state, stack, "no-dir")?;
        let no_files = call.has_flag(engine_state, stack, "no-file")?;

        let pattern_str = match pattern_value {
            Value::String { val, .. } | Value::Glob { val, .. } => val,
            _ => {
                return Err(ShellError::IncorrectValue {
                    msg: "Incorrect glob pattern supplied to glob. Please use string or glob only."
                        .to_string(),
                    val_span: call.head,
                    call_span: pattern_span,
                });
            }
        };

        if pattern_str.is_empty() {
            return Err(ShellError::GenericError {
                error: "glob pattern must not be empty".into(),
                msg: "glob pattern is empty".into(),
                span: Some(pattern_span),
                help: Some("add characters to the glob pattern".into()),
                inner: vec![],
            });
        }

        // Determine if pattern is absolute (starts with /)
        let is_absolute = pattern_str.starts_with('/');
        let base_path = if is_absolute { get_vfs() } else { get_pwd() };

        // Use the glob_match function
        let options = GlobOptions {
            max_depth: depth.map(|d| d as usize),
            no_dirs,
            no_files,
        };

        let matches = glob_match(&pattern_str, base_path, options)?;

        // Convert matches to Value stream
        let signals = engine_state.signals().clone();
        let values = matches
            .into_iter()
            .map(move |path| Value::string(path, span));

        Ok(PipelineData::list_stream(
            ListStream::new(values, span, signals.clone()),
            None,
        ))
    }
}
