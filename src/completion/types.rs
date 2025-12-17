use nu_protocol::Span;
use serde::Serialize;

#[derive(Debug, Serialize)]
pub struct Suggestion {
    pub name: String,
    pub description: Option<String>,
    pub is_command: bool,
    pub rendered: String,
    pub span_start: usize, // char index (not byte)
    pub span_end: usize,   // char index (not byte)
}

impl PartialEq for Suggestion {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}
impl Eq for Suggestion {}
impl PartialOrd for Suggestion {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.name.partial_cmp(&other.name)
    }
}
impl Ord for Suggestion {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.name.cmp(&other.name)
    }
}

#[derive(Debug)]
pub enum CompletionContext {
    Command {
        prefix: String,
        span: Span,
        parent_command: Option<String>, // If Some, only show subcommands of this command
    },
    Argument {
        prefix: String,
        span: Span,
    },
    Flag {
        prefix: String,
        span: Span,
        command_name: String,
    },
    CommandArgument {
        prefix: String,
        span: Span,
        command_name: String,
        arg_index: usize,
    },
    Variable {
        prefix: String, // without the $ prefix
        span: Span,
    },
    CellPath {
        prefix: String,             // the partial field name being typed (after the last dot)
        span: Span,                 // replacement span
        var_id: nu_protocol::VarId, // variable ID for evaluation
        path_so_far: Vec<String>,   // path members accessed before current one
    },
}
