use crate::completion::helpers::to_char_span;
use crate::completion::types::Suggestion;
use nu_protocol::Span;

pub fn generate_file_suggestions(
    prefix: &str,
    span: Span,
    root: &std::sync::Arc<vfs::VfsPath>,
    description: Option<String>,
    input: &str,
) -> Vec<Suggestion> {
    let (dir, file_prefix) = prefix
        .rfind('/')
        .map(|idx| (&prefix[..idx + 1], &prefix[idx + 1..]))
        .unwrap_or(("", prefix));

    let dir_to_join = (dir.len() > 1 && dir.ends_with('/'))
        .then(|| &dir[..dir.len() - 1])
        .unwrap_or(dir);

    let target_dir = if !dir.is_empty() {
        match root.join(dir_to_join) {
            Ok(d) if d.is_dir().unwrap_or(false) => Some(d),
            _ => None,
        }
    } else {
        Some(root.join("").unwrap())
    };

    let mut file_suggestions = Vec::new();
    if let Some(d) = target_dir {
        if let Ok(iterator) = d.read_dir() {
            let char_span = to_char_span(input, span);
            for entry in iterator {
                let name = entry.filename();
                if name.starts_with(file_prefix) {
                    let full_completion = format!("{}{}", dir, name);
                    file_suggestions.push(Suggestion {
                        name: full_completion.clone(),
                        description: description.clone(),
                        is_command: false,
                        rendered: full_completion,
                        span_start: char_span.start,
                        span_end: char_span.end,
                    });
                }
            }
        }
    }
    file_suggestions
}
