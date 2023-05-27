// region: Token location.
/// Range of a token in the source code. Represented by the start and end
/// byte offsets.
#[derive(Debug, PartialEq)]
pub struct Span {
    start: usize,
    end: usize,
}

impl Span {
    /// Creates a new span.
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

/// A token/AST node together with its [Span].
#[derive(Debug, PartialEq)]
pub struct Spanned<T>(T, Span);

impl<T> Spanned<T> {
    /// Creates a new spanned object.
    pub fn new(t: T, span: Span) -> Self {
        Self(t, span)
    }
}
// endregion

// region: Redirections.
/// Token that performs a redirection function.
#[derive(Debug, PartialEq)]
pub enum RedirOp {
    /// `>|`
    Clobber,
    /// `>>`
    DGreat,
    /// `<<`
    DLess,
    /// `<<-`
    DLessDash,
    /// `>`
    Great,
    /// `>&`
    GreatAnd,
    /// `<`
    Less,
    /// `<&`
    LessAnd,
    /// `<>`
    LessGreat,
}
// endregion

// region: Commands.
#[derive(Debug, PartialEq)]
/// Token that performs a control function.
pub enum ControlOp {
    /// `&`
    And,
    /// `&&`
    AndIf,
    /// `;;`
    DSemi,
    /// `(`
    LParen,
    /// `\n`
    Newline,
    /// `|`
    Or,
    /// `||`
    OrIf,
    /// `)`
    RParen,
    /// `;`
    Semi,
}
// endregion
