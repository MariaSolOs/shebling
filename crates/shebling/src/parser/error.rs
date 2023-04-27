use crate::{
    ast::Location,
    parser::{lint::Lint, Span},
};

#[derive(Debug, PartialEq)]
pub(crate) struct Note {
    message: &'static str,
    location: Location,
}

impl Note {
    pub(crate) fn message(&self) -> &'static str {
        self.message
    }

    pub(crate) fn location(&self) -> &Location {
        &self.location
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct Error {
    location: Location,
    lints: Vec<Lint>,
    notes: Vec<Note>,
}

impl Error {
    pub(super) fn location(&self) -> &Location {
        &self.location
    }

    // TODO: Find a better way (shared with context.rs) to display diagnostics.
    pub(super) fn lints(&self) -> &[Lint] {
        &self.lints
    }

    pub(super) fn notes(&self) -> &[Note] {
        &self.notes
    }
}

impl nom::error::ParseError<Span<'_>> for Error {
    fn from_error_kind(input: Span<'_>, _kind: nom::error::ErrorKind) -> Self {
        Self {
            location: Location::from(&input),
            lints: input.extra.take_lints(),
            notes: vec![],
        }
    }

    fn append(_input: Span<'_>, _kind: nom::error::ErrorKind, other: Self) -> Self {
        other
    }
}

impl nom::error::ContextError<Span<'_>> for Error {
    fn add_context(input: Span<'_>, ctx: &'static str, mut other: Self) -> Self {
        other.notes.push(Note {
            message: ctx,
            location: Location::from(input),
        });

        other
    }
}
