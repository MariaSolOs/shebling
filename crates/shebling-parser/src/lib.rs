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


#[macro_use]
mod token;

mod expr;

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
    MisplacedChar(&'static str, #[label("{0}")] Range),

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
            Self::MisplacedChar(_, range) | Self::Unichar(_, range) => range,
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
    let diags = trivia::trivia1(source_to_span(source_code))
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
