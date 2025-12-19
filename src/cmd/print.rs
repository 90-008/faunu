use crate::globals::print_to_console;
use nu_engine::CallExt;
use nu_protocol::{
    Category, PipelineData, ShellError, Signature, SyntaxShape, Type, Value,
    engine::{Command, EngineState, Stack},
};

#[derive(Clone)]
pub struct Print;

impl Command for Print {
    fn name(&self) -> &str {
        "print"
    }

    fn signature(&self) -> Signature {
        Signature::build("print")
            .rest("rest", SyntaxShape::Any, "values to print")
            .input_output_type(Type::Nothing, Type::Nothing)
            .category(Category::Strings)
    }

    fn description(&self) -> &str {
        "print values to the console."
    }

    fn run(
        &self,
        engine_state: &EngineState,
        stack: &mut Stack,
        call: &nu_protocol::engine::Call,
        _input: PipelineData,
    ) -> Result<PipelineData, ShellError> {
        let rest: Vec<Value> = call.rest(engine_state, stack, 0)?;

        let mut parts = Vec::new();
        for value in rest {
            let s = value.to_expanded_string(" ", &engine_state.config);
            parts.push(s);
        }
        let output = parts.join(" ");
        print_to_console(&output, true);

        Ok(PipelineData::Empty)
    }
}
