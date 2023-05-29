use crate::span::ParseSpan;

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
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
            location: 0,
            notes: vec![],
        }
    }

    fn append(_input: ParseSpan, _kind: nom::error::ErrorKind, other: Self) -> Self {
        other
    }
}

#[derive(Clone, Debug, thiserror::Error, miette::Diagnostic)]
#[error("{note}")]
#[diagnostic(severity("error"))]
pub(crate) struct ParseErrorNote {
    #[label]
    location: usize,
    note: &'static str,
}
