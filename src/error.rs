use miette::{Diagnostic, GraphicalReportHandler, Report, SourceCode, SourceSpan, SpanContents};
use nu_protocol::{ShellError, Span};
use vfs::{VfsError, error::VfsErrorKind};

pub struct CommandError {
    pub error: Report,
    pub start_offset: usize,
    pub input: String,
}

impl CommandError {
    pub fn new<E>(error: E, input: impl Into<String>) -> Self
    where
        E: Diagnostic + Clone + Send + Sync + 'static,
    {
        Self {
            error: Report::new(error),
            start_offset: 0,
            input: input.into(),
        }
    }

    pub fn with_start_offset(mut self, start_offset: usize) -> Self {
        self.start_offset = start_offset;
        self
    }
}

impl From<ShellError> for CommandError {
    fn from(value: ShellError) -> Self {
        CommandError::new(value, String::new())
    }
}

impl From<CommandError> for String {
    fn from(value: CommandError) -> Self {
        let handler = GraphicalReportHandler::new()
            .with_theme(miette::GraphicalTheme::unicode())
            .with_cause_chain();

        if value.input.is_empty() {
            let mut msg = String::new();
            handler
                .render_report(&mut msg, value.error.as_ref())
                .unwrap();
            return msg;
        }

        let source = OffsetSource {
            source: value.input,
            start_offset: value.start_offset,
        };

        let report_with_source = value.error.with_source_code(source);
        let mut msg = String::new();
        handler
            .render_report(&mut msg, report_with_source.as_ref())
            .unwrap();
        msg
    }
}

pub struct OffsetSource {
    pub source: String,
    pub start_offset: usize,
}

pub struct OffsetSpanContents<'a> {
    inner: Box<dyn SpanContents<'a> + 'a>,
    global_span: SourceSpan,
}

impl<'a> SpanContents<'a> for OffsetSpanContents<'a> {
    fn data(&self) -> &'a [u8] {
        self.inner.data()
    }
    fn span(&self) -> &SourceSpan {
        &self.global_span
    }
    fn line(&self) -> usize {
        self.inner.line()
    }
    fn column(&self) -> usize {
        self.inner.column()
    }
    fn line_count(&self) -> usize {
        self.inner.line_count()
    }
}

impl SourceCode for OffsetSource {
    fn read_span<'b>(
        &'b self,
        span: &SourceSpan,
        context_lines_before: usize,
        context_lines_after: usize,
    ) -> Result<Box<dyn miette::SpanContents<'b> + 'b>, miette::MietteError> {
        let local_start = span.offset().saturating_sub(self.start_offset);
        let local_len = std::cmp::min(span.len(), self.source.len().saturating_sub(local_start));
        let local_span = SourceSpan::new(local_start.into(), local_len);

        let local_contents =
            self.source
                .read_span(&local_span, context_lines_before, context_lines_after)?;

        let content_local_span = local_contents.span();
        let global_start = content_local_span.offset() + self.start_offset;
        let global_span = SourceSpan::new(global_start.into(), content_local_span.len());

        Ok(Box::new(OffsetSpanContents {
            inner: local_contents,
            global_span,
        }))
    }
}

pub fn to_shell_err(span: Span) -> impl Fn(VfsError) -> ShellError {
    move |vfs_error: VfsError| ShellError::GenericError {
        error: (match vfs_error.kind() {
            VfsErrorKind::DirectoryExists
            | VfsErrorKind::FileExists
            | VfsErrorKind::FileNotFound
            | VfsErrorKind::InvalidPath => "path error",
            _ => "io error",
        })
        .to_string(),
        msg: vfs_error.to_string(),
        span: Some(span),
        help: None,
        inner: vec![],
    }
}
