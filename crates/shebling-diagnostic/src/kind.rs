#[derive(Debug, miette::Diagnostic, thiserror::Error)]
pub enum DiagnosticKind {
    #[error("bad escaping!")]
    #[diagnostic(code(shebling::bad_escape))]
    BadEscape,

    #[error("this character looks a bit suspicious")]
    #[diagnostic(code(shebling::sus_char))]
    SusChar,
}
