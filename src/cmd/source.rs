use crate::globals::{get_pwd, queue_delta, to_shell_err};
use nu_engine::{CallExt, eval_block};
use nu_parser::parse;
use nu_protocol::{
    Category, PipelineData, ShellError, Signature, Span, SyntaxShape, Type,
    debugger::WithoutDebug,
    engine::{Command, EngineState, Stack, StateWorkingSet},
};
use vfs::VfsPath;

#[derive(Clone)]
pub struct Source;

impl Command for Source {
    fn name(&self) -> &str {
        "source-file"
    }

    fn signature(&self) -> Signature {
        Signature::build(self.name())
            .required("path", SyntaxShape::Filepath, "the file to source")
            .input_output_type(Type::Nothing, Type::Nothing)
            .category(Category::Core)
    }

    fn description(&self) -> &str {
        "source a file from the virtual filesystem."
    }

    fn run(
        &self,
        engine_state: &EngineState,
        stack: &mut Stack,
        call: &nu_protocol::engine::Call,
        _input: PipelineData,
    ) -> Result<PipelineData, ShellError> {
        let path: String = call.req(engine_state, stack, 0)?;
 
        let path = get_pwd()
            .join(&path)
            .map_err(to_shell_err(call.arguments_span()))?;
        eval_file(engine_state, stack, &path, call.arguments_span())
    }
}

pub fn eval_file<'s>(
    engine_state: &'s EngineState,
    stack: &mut Stack,
    path: &VfsPath,
    span: Span,
) -> Result<PipelineData, ShellError> {
    let contents = path.read_to_string().map_err(to_shell_err(span))?;

    let mut working_set= StateWorkingSet::new(engine_state);
    let _ = working_set.add_file(path.filename(), contents.as_bytes());

    // we dont need the block here just the delta
    let block = parse(
        &mut working_set,
        Some(&path.filename()),
        contents.as_bytes(),
        false,
    );

    if let Some(err) = working_set.parse_errors.first() {
        web_sys::console::error_1(&err.to_string().into());
        return Err(ShellError::GenericError {
            error: "parse error".into(),
            msg: err.to_string(),
            span: Some(span),
            help: None,
            inner: vec![],
        });
    }
    if let Some(err) = working_set.compile_errors.first() {
        web_sys::console::error_1(&err.to_string().into());
        return Err(ShellError::GenericError {
            error: "compile error".into(),
            msg: err.to_string(),
            span: Some(span),
            help: None,
            inner: vec![],
        });
    }

    queue_delta(working_set.delta);

    eval_block::<WithoutDebug>(engine_state, stack, &block, PipelineData::Empty).map(|d| d.body)
}
