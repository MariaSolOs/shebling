// TODO: Remove this after development.
#![allow(dead_code)]

use miette::{Diagnostic, Report};
// We import nom's parsers here so that we don't need to import
// them in every file.
use nom::{
    branch::alt,
    bytes::complete::{is_not, tag},
    character::complete::{char, newline, one_of},
    combinator::{map, opt, recognize, value, verify},
    error::context,
    multi::{many0, many1},
    sequence::{pair, preceded, tuple},
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
            let start = $diag.range().start;
            let end = $diag.range().end;
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
            $(, [ $( (($line1:literal, $col1:literal), $(($line2:literal, $col2:literal),)? $code:literal )),+ ] )?
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
                    assert_diag_eq!(diag, (($line1, $col1), $( ($line2, $col2), )? $code));
                }
            )+)?
        };

        ($parser:ident($source:literal) => Err($line:literal, $col:literal)) => {
            assert_parse!($parser($source) => Err(($line, $col), Notes: []));
        };

        ($parser:ident($source:literal) => Err(
            ($line1:literal, $col1:literal),
            Notes: [ $( (($line2:literal, $col2:literal), $note:literal) ),* ]
        )) => {
            let err = $parser(source_to_span($source))
                .finish()
                .expect_err("Parser should fail.");

            // Check the error location.
            ::pretty_assertions::assert_eq!(err.location.line, $line1);
            ::pretty_assertions::assert_eq!(err.location.column, $col1);

            // For flexibility, just check that the notes have the expected messages.
            let mut notes = err.notes.into_iter();
            $(
                let note = notes.next().expect("Expected a parser note.");
                let (note, location) = (note.note, note.location);
                ::pretty_assertions::assert_eq!(location.line, $line2);
                ::pretty_assertions::assert_eq!(location.column, $col2);
                assert!(note.contains($note), "Expected \"{}\" to contain \"{}\"", note, $note);
            )*
            let last_note = notes.next();
            assert!(last_note.is_none(), "There's a note left: {:#?}", last_note);
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
    #[error("Misplaced character!")]
    #[diagnostic(code(shebling::misplaced_char), severity("warning"))]
    MisplacedChar(
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
    fn range(&self) -> &Range {
        match self {
            Self::MisplacedChar(_, range, _) | Self::Unichar(_, range) => range,
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

pub fn parse(source_code: &str) {
    let diags = line_continuation(source_to_span(source_code))
        .finish()
        .unwrap()
        .0
        .extra
        .take_diags();

    let source_code: Arc<str> = source_code.into();
    for diag in diags {
        println!(
            "{:?}",
            Report::new(diag).with_source_code(Arc::clone(&source_code))
        );
    }
}
