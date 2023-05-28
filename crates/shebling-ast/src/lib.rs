// region: Token location
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

// region: Words
/// Sequence of [word segments](WordSgmt) treated as a unit by the shell.
#[derive(Debug)]
pub struct Word(Vec<Spanned<WordSgmt>>);

impl Word {
    /// Creates a new [Word] containing the given [segments](WordSgmt).
    pub fn new(sgmts: Vec<Spanned<WordSgmt>>) -> Self {
        Self(sgmts)
    }
}

#[derive(Debug)]
pub enum WordSgmt {
    /// Single quoted string.
    SingleQuoted { string: String, ansi_c_quoted: bool },
}
// endregion

// region: Redirections
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

// region: Commands
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
