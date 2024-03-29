use thiserror::Error;

use crate::{
    diagnostic::ParseDiagnostic,
    location::{Location, Span},
};

#[derive(Debug, Error, miette::Diagnostic)]
#[error("parser bailed!")]
#[diagnostic(code(shebling::parser::fatal), severity("error"))]
pub(crate) struct ParseError {
    #[label("stopped here")]
    location: Location,
    #[related]
    notes: Vec<ParseErrorNote>,
    diags: Vec<ParseDiagnostic>,
}

impl ParseError {
    pub(crate) fn line(&self) -> u32 {
        self.location.line()
    }

    pub(crate) fn column(&self) -> usize {
        self.location.column()
    }

    pub(crate) fn notes(&self) -> &[ParseErrorNote] {
        &self.notes
    }

    pub(crate) fn diags(&self) -> &[ParseDiagnostic] {
        &self.diags
    }
}

#[derive(Clone, Debug, miette::Diagnostic, Error)]
#[error("{note}")]
#[diagnostic(severity("error"))]
pub(crate) struct ParseErrorNote {
    #[label]
    location: Location,
    note: &'static str,
}

impl ParseErrorNote {
    pub(crate) fn line(&self) -> u32 {
        self.location.line()
    }

    pub(crate) fn column(&self) -> usize {
        self.location.column()
    }

    pub(crate) fn note(&self) -> &'static str {
        self.note
    }
}

impl nom::error::ParseError<Span<'_>> for ParseError {
    fn from_error_kind(input: Span<'_>, _kind: nom::error::ErrorKind) -> Self {
        Self {
            location: Location::from(&input),
            notes: vec![],
            diags: input.extra.take_diags(),
        }
    }

    fn append(_input: Span<'_>, _kind: nom::error::ErrorKind, other: Self) -> Self {
        other
    }
}

impl nom::error::ContextError<Span<'_>> for ParseError {
    fn add_context(input: Span<'_>, ctx: &'static str, mut other: Self) -> Self {
        other.notes.push(ParseErrorNote {
            location: Location::from(input),
            note: ctx,
        });

        other
    }
}
