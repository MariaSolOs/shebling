use derive_more::From;

use super::*;

/// Sequence of [word segments](WordSgmt) treated as a unit by the shell.
#[derive(Debug)]
pub struct Word(Vec<WordSgmt>);

impl Word {
    /// Creates a new [Word] containing the given [segments](WordSgmt).
    pub fn new(sgmts: Vec<WordSgmt>) -> Self {
        Self(sgmts)
    }

    /// Returns a reference to this [Word]'s segments.
    pub fn sgmts(&self) -> &[WordSgmt] {
        &self.0
    }

    /// If this [Word] represents a literal string, returns the literal.
    ///
    /// A word is considered a literal if:
    /// - The word has a single segment.
    /// - Such segment is a [WordSgmt::Lit].
    pub fn as_lit(&self) -> Option<&Spanned<String>> {
        match &self.0.first() {
            Some(WordSgmt::Lit(lit)) if self.0.len() == 1 => Some(lit),
            _ => None,
        }
    }
}

#[derive(Debug, From)]
pub enum WordSgmt {
    /// Brace expansion.
    BraceExpansion(Vec<Word>),

    /// Dollar-prefixed expression.
    #[from]
    DollarExp(DollarExp),

    // Double-quoted string.
    #[from]
    DoubleQuoted(DoubleQuoted),

    /// Pattern match sequence.
    Glob(Spanned<String>),

    /// Literal, unquoted string.
    Lit(Spanned<String>),

    // Single-quoted string.
    SingleQuoted(Spanned<String>),
}

/// A sequence of [word segments][DoubleQuotedSgmt] enclosed in `""`.
#[derive(Debug)]
pub struct DoubleQuoted(Vec<DoubleQuotedSgmt>);

impl DoubleQuoted {
    /// Creates a new double-quoted string.
    pub fn new(sgmts: Vec<DoubleQuotedSgmt>) -> Self {
        Self(sgmts)
    }
}

#[derive(Debug, From)]
pub enum DoubleQuotedSgmt {
    // TODO: BackQuoted(Term),
    Lit(Spanned<String>),
    DollarExp(DollarExp),
}

/// A subscripted variable, used for indexing into [Array]s.
#[derive(Debug)]
pub struct SubscriptedVar {
    ident: Spanned<String>,
    // Left unparsed for now.
    subscripts: Vec<Spanned<String>>,
}

impl SubscriptedVar {
    /// Creates a new subscripted variable with the given identifier
    /// and subscripts.
    pub fn new(ident: Spanned<String>, subscripts: Vec<Spanned<String>>) -> Self {
        Self { ident, subscripts }
    }
}
