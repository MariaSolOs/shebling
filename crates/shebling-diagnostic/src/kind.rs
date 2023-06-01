#[derive(Debug, miette::Diagnostic, thiserror::Error)]
pub enum DiagnosticKind {
    #[error("bad escaping!")]
    #[diagnostic(code(shebling::bad_escape))]
    BadEscape,

    #[error("bad quote!")]
    #[diagnostic(code(shebling::bad_quote))]
    BadQuote,
}
