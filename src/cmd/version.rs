use nu_protocol::Type;
use nu_protocol::engine::Call;
use nu_protocol::{
    Category, IntoPipelineData, PipelineData, ShellError, Signature, Value,
    engine::{Command, EngineState, Stack},
};

#[derive(Clone)]
pub struct Version;

impl Command for Version {
    fn name(&self) -> &str {
        "version"
    }

    fn signature(&self) -> Signature {
        Signature::build(self.name())
            .input_output_type(Type::Nothing, Type::String)
            .category(Category::System)
    }

    fn description(&self) -> &str {
        "print the version of dysnomia."
    }

    fn run(
        &self,
        _engine_state: &EngineState,
        _stack: &mut Stack,
        call: &Call,
        _input: PipelineData,
    ) -> Result<PipelineData, ShellError> {
        let date = compile_time::unix!();
        let rustc = compile_time::rustc_version_str!();
        Ok(
            Value::string(format!("dysnomia.v099.t{date} (rustc {rustc})"), call.head)
                .into_pipeline_data(),
        )
    }
}
