use super::*;

/// Sequence of [word segments](WordSgmt) treated as a unit by the shell.
pub struct Word(Vec<WordSgmt>);

#[derive(Debug)]
pub enum WordSgmt {
    SingleQuoted(Spanned<String>),
}

/// A sequence of [word segments][DoubleQuotedSgmt] enclosed in `""`.
pub struct DoubleQuoted(Vec<DoubleQuotedSgmt>);

impl DoubleQuoted {
    /// Creates a new double-quoted string.
    pub fn new(sgmts: Vec<DoubleQuotedSgmt>) -> Self {
        Self(sgmts)
    }
}

pub enum DoubleQuotedSgmt {
    Lit(Spanned<String>),
}
