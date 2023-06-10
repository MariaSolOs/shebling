use super::*;

/// Sequence of [word segments](WordSgmt) treated as a unit by the shell.
pub struct Word(Vec<WordSgmt>);

#[derive(Debug)]
pub enum WordSgmt {
    SingleQuoted(Spanned<String>),
}
