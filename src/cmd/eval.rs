use crate::globals::print_to_console;
use nu_engine::CallExt;
use nu_protocol::engine::Call;
use nu_protocol::{
    Category, PipelineData, ShellError, Signature,
    engine::{Command, EngineState, Stack},
};
use nu_protocol::{SyntaxShape, Type};

#[derive(Clone)]
pub struct Eval;

impl Command for Eval {
    fn name(&self) -> &str {
        "eval"
    }

    fn signature(&self) -> Signature {
        Signature::build(self.name())
            .optional("code", SyntaxShape::String, "code to evaluate")
            .input_output_type(Type::one_of([Type::Nothing, Type::String]), Type::Nothing)
            .category(Category::FileSystem)
    }

    fn description(&self) -> &str {
        "evaluates a string as nushell code."
    }

    fn run(
        &self,
        engine_state: &EngineState,
        stack: &mut Stack,
        call: &Call,
        input: PipelineData,
    ) -> Result<PipelineData, ShellError> {
        let code: Option<String> = call.opt(engine_state, stack, 0)?;

        let (span, code) = match code {
            Some(c) => (Some(call.arguments_span()), c),
            None => (
                input.span(),
                input.collect_string("\n", &engine_state.config)?,
            ),
        };

        match super::source_file::eval(engine_state, stack, &code, None) {
            Ok(d) => Ok(d),
            Err(err) => {
                let msg: String = err.into();
                print_to_console(&msg, true);
                Err(ShellError::GenericError {
                    error: "source error".into(),
                    msg: "can't source string".into(),
                    span,
                    help: None,
                    inner: vec![],
                })
            }
        }
    }
}
