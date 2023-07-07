use crate::span::ParseSpan;

/// A `shebling` parse error.
#[derive(Debug, miette::Diagnostic, thiserror::Error)]
#[error("parser bailed!")]
#[diagnostic(code(shebling::parser::fatal), severity("error"))]
pub(crate) struct ParseError {
    #[label("stopped here")]
    location: usize,
    #[related]
    notes: Vec<ParseErrorNote>,
}

impl nom::error::ParseError<ParseSpan<'_>> for ParseError {
    fn from_error_kind(input: ParseSpan, _kind: nom::error::ErrorKind) -> Self {
        Self {
            location: input.offset(),
            notes: vec![],
        }
    }

    fn append(_input: ParseSpan, _kind: nom::error::ErrorKind, other: Self) -> Self {
        other
    }
}

impl nom::error::ContextError<ParseSpan<'_>> for ParseError {
    fn add_context(input: ParseSpan<'_>, ctx: &'static str, mut other: Self) -> Self {
        other.notes.push(ParseErrorNote {
            location: input.offset(),
            note: ctx,
        });

        other
    }
}

/// Extra context information about a `shebling` parse error.
#[derive(Debug, miette::Diagnostic, thiserror::Error)]
#[error("{note}")]
#[diagnostic(severity("error"))]
pub(crate) struct ParseErrorNote {
    #[label]
    location: usize,
    note: &'static str,
}
