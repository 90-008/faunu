use nu_protocol::Span;
use serde::Serialize;

#[derive(Debug, Serialize)]
pub struct Suggestion {
    pub name: String,
    pub description: Option<String>,
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

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CompletionKind {
    Command {
        parent_command: Option<String>, // If Some, only show subcommands of this command
    },
    Argument,
    Flag {
        command_name: String,
    },
    CommandArgument {
        command_name: String,
        arg_index: usize,
    },
    Variable, // prefix is without the $ prefix
    CellPath {
        var_id: nu_protocol::VarId, // variable ID for evaluation
        path_so_far: Vec<String>,   // path members accessed before current one
    },
}

#[derive(Debug)]
pub struct CompletionContext {
    pub kind: CompletionKind,
    pub prefix: String, // the partial text being completed
    pub span: Span,
}

impl PartialEq for CompletionContext {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind && self.prefix == other.prefix
    }
}

impl Eq for CompletionContext {}

impl PartialOrd for CompletionContext {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match self.kind.partial_cmp(&other.kind) {
            Some(std::cmp::Ordering::Equal) => self.prefix.partial_cmp(&other.prefix),
            other => other,
        }
    }
}

impl Ord for CompletionContext {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.kind.cmp(&other.kind) {
            std::cmp::Ordering::Equal => self.prefix.cmp(&other.prefix),
            other => other,
        }
    }
}

impl std::hash::Hash for CompletionContext {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.kind.hash(state);
        self.prefix.hash(state);
    }
}
