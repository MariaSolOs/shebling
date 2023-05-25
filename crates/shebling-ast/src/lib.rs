use std::fmt;

// TODO: Document.

// region: Token location.
#[derive(PartialEq)]
pub struct Span {
    start: usize,
    end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

#[derive(Debug, PartialEq)]
pub struct Spanned<T>(T, Span);

impl<T> Spanned<T> {
    pub fn new(token: T, span: Span) -> Self {
        Self(token, span)
    }

    pub fn token(&self) -> &T {
        &self.0
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
