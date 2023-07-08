use derive_more::From;

use super::*;

/// Sequence of [word segments](WordSgmt) treated as a unit by the shell.
pub struct Word(Vec<WordSgmt>);

#[derive(Debug, From)]
pub enum WordSgmt {
    // Double-quoted string.
    #[from]
    DoubleQuoted(DoubleQuoted),

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
    Lit(Spanned<String>),
}

/// A subscripted variable, used for indexing into [Array]s.
#[derive(Debug)]
pub struct SubscriptedVar {
    ident: String,
    // Left unparsed for now.
    subscripts: Vec<String>,
}

impl SubscriptedVar {
    /// Creates a new subscripted variable with the given identifier
    /// and subscripts.
    pub fn new(ident: impl Into<String>, subscripts: Vec<String>) -> Self {
        Self {
            ident: ident.into(),
            subscripts,
        }
    }
}
