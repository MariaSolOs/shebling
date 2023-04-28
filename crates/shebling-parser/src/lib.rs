// TODO: Remove this after development.
#![allow(dead_code)]

use miette::{Diagnostic, Report};
// We import nom's parsers here so that we don't need to import
// them in every file.
use nom::{
    branch::alt,
    bytes::complete::{is_not, tag},
    character::complete::{char, newline, one_of},
    combinator::{map, opt, recognize, value},
    multi::{many0, many1},
    sequence::{pair, preceded, tuple},
};
use nom_locate::position;
use shebling_codegen::New;
use std::borrow::Borrow;
use std::cell::RefCell;
use thiserror::Error;

mod quoted;
pub(crate) use quoted::line_continuation;

mod trivia;

type ParseResult<'a, R> = nom::IResult<Span<'a>, R, ParseError>;

// region: Symbol location.
type Span<'a> = nom_locate::LocatedSpan<&'a str, ParseContext>;

#[derive(Clone, Copy, Debug, New)]
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
        let value: &Span = value.borrow();
        Self::new(
            value.location_offset(),
            value.location_line(),
            value.get_utf8_column(),
        )
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

// region: Parsing context.
#[derive(Clone, Debug)]
struct ParseContext {
    diags: RefCell<Vec<ParseDiagnostic>>,
}

impl ParseContext {
    fn report(&self, diag: ParseDiagnostic) {
        self.diags.borrow_mut().push(diag);
    }
}

#[derive(Clone, Debug, Diagnostic, Error)]
enum ParseDiagnostic {
    #[error("Misplaced character!")]
    #[diagnostic(code(shebling::misplaced_char), severity("warning"))]
    MisplacedChar(String, #[label("{0}")] Range),

    #[error("Unicode character!")]
    #[diagnostic(
        code(shebling::unichar),
        help("Delete and retype it."),
        severity("warning")
    )]
    Unichar(String, #[label("unicode {0}")] Range),
}

impl ParseDiagnostic {
    fn into_report(self, source_code: String) -> Report {
        Report::new(self).with_source_code(source_code)
    }
}

#[derive(Debug)]
struct ParseError {
    location: Location,
}

impl nom::error::ParseError<Span<'_>> for ParseError {
    fn from_error_kind(input: Span<'_>, _kind: nom::error::ErrorKind) -> Self {
        Self {
            location: Location::from(input),
        }
    }

    fn append(_input: Span<'_>, _kind: nom::error::ErrorKind, other: Self) -> Self {
        other
    }
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
