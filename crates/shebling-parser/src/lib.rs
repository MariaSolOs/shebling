// TODO: Remove this after development.
#![allow(dead_code, unused_imports)]

use miette::{Diagnostic, Report};
// We import nom's parsers here so that we don't need to import
// them in every file.
use nom::{
    branch::alt,
    bytes::complete::{is_not, tag},
    character::complete::{char, newline, one_of},
    combinator::{consumed, into, map, not, opt, peek, recognize, value, verify},
    error::context,
    multi::{many0, many1, separated_list0},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    Finish,
};
use nom_locate::position;
use shebling_codegen::New;
use std::cell::RefCell;
use std::{borrow::Borrow, sync::Arc};
use thiserror::Error;

#[cfg(test)]
#[macro_use]
mod tests {
    macro_rules! assert_diag_eq {
        ($diag:expr, (($line:literal, $col:literal), $code:literal)) => {
            assert_diag_eq!($diag, (($line, $col), ($line, $col), $code))
        };

        ($diag:expr, (($line1:literal, $col1:literal), ($line2:literal, $col2:literal), $code:literal)) => {
            // Check that the codes match.
            let code = $diag
                .code()
                .expect("Diagnostic should have a code.")
                .to_string()
                .strip_prefix("shebling::")
                .expect("All codes should start with shebling::")
                .to_owned();
            ::pretty_assertions::assert_str_eq!(code, $code);

            // Check the range coordinates.
            let Range { start, end } = $diag.range();
            ::pretty_assertions::assert_eq!(start.line, $line1);
            ::pretty_assertions::assert_eq!(start.column, $col1);
            ::pretty_assertions::assert_eq!(end.line, $line2);
            ::pretty_assertions::assert_eq!(end.column, $col2);
        };
    }

    macro_rules! assert_parse {
        (
            $parser:ident($source:literal) => $unparsed:literal,
            $res:expr
            $(, [ $( (($line1:literal, $col1:literal), $(($line2:literal, $col2:literal),)? $code:literal) ),+ ] )?
        ) => {
            let (span, res) = $parser(source_to_span($source))
                .finish()
                .expect("Parser should succeed.");

            // Verify the result.
            ::pretty_assertions::assert_eq!(res, $res);

            // Verify the unparsed content.
            ::pretty_assertions::assert_str_eq!(*span.fragment(), $unparsed);

            // Verify the diagnostics.
            $($(
                for diag in span.extra.take_diags() {
                    assert_diag_eq!(diag, (($line1, $col1), $(($line2, $col2),)? $code));
                }
            )+)?
        };

        ($parser:ident($source:literal) => Err($line:literal, $col:literal)) => {
            assert_parse!($parser($source) => Err(($line, $col), Notes: [], Diags: []));
        };

        ($parser:ident($source:literal) => Err(
            ($line:literal, $col:literal),
            Notes: [ $( (($line1:literal, $col1:literal), $note:literal) ),+ ]
        )) => {
            assert_parse!($parser($source) => Err(
                ($line, $col),
                Notes: [ $( (($line1, $col1), $note) ),+ ],
                Diags: []
            ));
        };

        ($parser:ident($source:literal) => Err(
            ($line:literal, $col:literal),
            Diags: [ $( (($line1:literal, $col1:literal), $(($line2:literal, $col2:literal),)? $code:literal) ),+ ]
        )) => {
            assert_parse!($parser($source) => Err(
                ($line, $col),
                Notes: [],
                Diags: [ $( (($line1, $col1), $(($line2, $col2),)? $code) ),+ ]
            ));
        };

        ($parser:ident($source:literal) => Err(
            ($line:literal, $col:literal),
            Notes: [ $( (($line1:literal, $col1:literal), $note:literal) ),* ],
            Diags: [ $( (($line2:literal, $col2:literal), $(($line3:literal, $col3:literal),)? $code:literal) ),* ]
        )) => {
            let err = $parser(source_to_span($source))
                .finish()
                .expect_err("Parser should fail.");

            // Check the error location.
            ::pretty_assertions::assert_eq!(err.location.line, $line);
            ::pretty_assertions::assert_eq!(err.location.column, $col);

            // For flexibility, just check that the notes have the expected messages.
            let mut notes = err.notes.into_iter();
            $(
                let ParseErrorNote { note, location } = notes.next().expect("Expected a parser note.");
                ::pretty_assertions::assert_eq!(location.line, $line1);
                ::pretty_assertions::assert_eq!(location.column, $col1);
                assert!(note.contains($note), "Expected \"{}\" to contain \"{}\"", note, $note);
            )*
            let last_note = notes.next();
            assert!(last_note.is_none(), "There's a note left: {:#?}", last_note);

            // Check the diagnostics.
            $(
                for diag in err.diags {
                    assert_diag_eq!(diag, (($line2, $col2), $(($line3, $col3),)? $code));
                }
            )*
        };
    }
}

#[macro_use]
mod token;
mod ast;
pub(crate) use ast::*;

mod expansion;

mod quoted;
pub(crate) use quoted::line_continuation;

mod trivia;

// region: Symbol location.
type Span<'a> = nom_locate::LocatedSpan<&'a str, ParseContext>;

#[derive(Clone, Copy, Debug)]
struct Location {
    offset: usize,
    line: u32,
    column: usize,
}

impl<'a, S> From<S> for Location
where
    S: Borrow<Span<'a>>,
{
    fn from(value: S) -> Self {
        let value = value.borrow();
        Self {
            offset: value.location_offset(),
            line: value.location_line(),
            column: value.get_utf8_column(),
        }
    }
}

impl From<Location> for miette::SourceSpan {
    fn from(value: Location) -> Self {
        value.offset.into()
    }
}

#[derive(Clone, Copy, Debug, New)]
struct Range {
    #[new(into)]
    start: Location,
    #[new(into)]
    end: Location,
}

impl<L: Into<Location>> From<L> for Range {
    fn from(value: L) -> Self {
        let location = value.into();
        Self::new(location, location)
    }
}

impl From<Range> for miette::SourceSpan {
    fn from(value: Range) -> Self {
        let offset = value.start.offset;
        let len = value.end.offset - offset;

        (offset, len).into()
    }
}
// endregion

// region: Parsing state.
type ParseResult<'a, R> = nom::IResult<Span<'a>, R, ParseError>;

#[derive(Clone, Debug)]
struct ParseContext {
    diags: RefCell<Vec<ParseDiagnostic>>,
}

impl ParseContext {
    fn diag(&self, diag: ParseDiagnostic) {
        self.diags.borrow_mut().push(diag);
    }

    fn take_diags(&self) -> Vec<ParseDiagnostic> {
        self.diags.take()
    }
}

#[derive(Clone, Debug, Diagnostic, Error)]
enum ParseDiagnostic {
    #[error("Unexpected character!")]
    #[diagnostic(code(shebling::unexpected_char), severity("warning"))]
    UnexpectedChar(
        &'static str,
        #[label("{0}")] Range,
        #[help] Option<&'static str>,
    ),

    #[error("Unicode character!")]
    #[diagnostic(
        code(shebling::unichar),
        help("Delete and retype it."),
        severity("warning")
    )]
    Unichar(&'static str, #[label("unicode {0}")] Range),
}

impl ParseDiagnostic {
    // TODO: Try to find an agreement with miette so that we can still
    // easily access line/column info from a source span.
    fn range(&self) -> &Range {
        match self {
            Self::UnexpectedChar(_, range, _) | Self::Unichar(_, range) => range,
        }
    }
}

#[derive(Debug, Diagnostic, Error)]
#[error("Parser bailed!")]
#[diagnostic(code("shebling::parser_error"))]
struct ParseError {
    #[label("stopped here")]
    location: Location,
    #[related]
    notes: Vec<ParseErrorNote>,
    diags: Vec<ParseDiagnostic>,
}

impl nom::error::ParseError<Span<'_>> for ParseError {
    fn from_error_kind(input: Span<'_>, _kind: nom::error::ErrorKind) -> Self {
        Self {
            location: Location::from(&input),
            notes: Vec::new(),
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

#[derive(Debug, Diagnostic, Error)]
#[error("{note}")]
struct ParseErrorNote {
    #[label("{note}")]
    location: Location,
    note: &'static str,
}
// endregion

// region: Shared utility parsers.
fn ranged<'a, P, R>(parser: P) -> impl FnMut(Span<'a>) -> ParseResult<(R, Range)>
where
    P: FnMut(Span<'a>) -> ParseResult<R>,
{
    map(tuple((position, parser, position)), |(start, res, end)| {
        (res, Range::new(start, end))
    })
}

fn recognize_string<'a, P, R>(parser: P) -> impl FnMut(Span<'a>) -> ParseResult<String>
where
    P: FnMut(Span<'a>) -> ParseResult<R>,
{
    map(recognize(parser), |span| (*span.fragment()).into())
}

fn swallow<'a, P, R>(parser: P) -> impl FnMut(Span<'a>) -> ParseResult<()>
where
    P: FnMut(Span<'a>) -> ParseResult<R>,
{
    value((), parser)
}
// endregion

fn source_to_span(source_code: &str) -> Span {
    let context = ParseContext {
        diags: RefCell::new(Vec::new()),
    };

    nom_locate::LocatedSpan::new_extra(source_code, context)
}

pub fn parse(file_path: impl AsRef<str>, source_code: &str) {
    let diags = line_continuation(source_to_span(source_code))
        .finish()
        .unwrap()
        .0
        .extra
        .take_diags();

    let source_code = Arc::new(miette::NamedSource::new(file_path, source_code.to_owned()));
    for diag in diags {
        println!(
            "{:?}",
            // TODO: Include file name.
            Report::new(diag).with_source_code(Arc::clone(&source_code))
        );
    }
}
