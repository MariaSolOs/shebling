// TODO: Remove this after development.
#![allow(dead_code, unused_imports)]

use miette::{Diagnostic, Report, Severity};
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
use std::{borrow::Borrow, sync::Arc};
use std::{cell::RefCell, fmt};
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
                let LabeledRange { range, label } = notes.next().expect("Expected a parser note.");
                ::pretty_assertions::assert_eq!(range.start.line, $line1);
                ::pretty_assertions::assert_eq!(range.start.column, $col1);
                assert!(label.contains($note), "Expected \"{}\" to contain \"{}\"", label, $note);
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
        let start = value.start.offset;
        let end = value.end.offset;

        (start, end).into()
    }
}
// endregion

// region: Parsing state.
type ParseResult<'a, R> = nom::IResult<Span<'a>, R, ParseError>;

#[derive(Clone, Debug, Diagnostic, Error)]
#[error("{label}")]
struct LabeledRange {
    #[label("{label}")]
    range: Range,
    label: &'static str,
}

#[derive(Clone, Debug)]
struct ParseContext {
    diags: RefCell<Vec<ParseDiagnostic>>,
}

impl ParseContext {
    fn diag(&self, builder: ParseDiagnosticBuilder) {
        self.diags.borrow_mut().push(builder.build());
    }

    fn take_diags(&self) -> Vec<ParseDiagnostic> {
        self.diags.take()
    }
}

#[derive(Clone, Debug, Error)]
#[error("{kind}")]
struct ParseDiagnostic {
    kind: ParseDiagnosticKind,
    labels: Vec<LabeledRange>,
    help: Option<&'static str>,
}

impl Diagnostic for ParseDiagnostic {
    fn code<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        self.kind.code()
    }

    fn severity(&self) -> Option<Severity> {
        self.kind.severity()
    }

    fn help<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        self.help
            .map(|help| Box::new(help) as Box<dyn fmt::Display>)
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = miette::LabeledSpan> + '_>> {
        Some(Box::new(self.labels.iter().map(|label| {
            miette::LabeledSpan::new_with_span(Some(label.label.into()), label.range)
        })))
    }
}

impl ParseDiagnostic {
    fn new(kind: ParseDiagnosticKind) -> ParseDiagnosticBuilder {
        ParseDiagnosticBuilder {
            kind,
            labels: Vec::new(),
            help: None,
        }
    }

    fn range(&self) -> &Range {
        &self
            .labels
            .first()
            .expect("Diagnostics should have at least one label.")
            .range
    }
}

struct ParseDiagnosticBuilder {
    kind: ParseDiagnosticKind,
    labels: Vec<LabeledRange>,
    help: Option<&'static str>,
}

impl ParseDiagnosticBuilder {
    fn label(mut self, label: &'static str, range: Range) -> Self {
        self.labels.push(LabeledRange { range, label });
        self
    }

    fn help(mut self, help: &'static str) -> Self {
        self.help = Some(help);
        self
    }

    fn build(self) -> ParseDiagnostic {
        ParseDiagnostic {
            kind: self.kind,
            labels: self.labels,
            help: self.help,
        }
    }
}

#[derive(Clone, Debug, Diagnostic, Error)]
enum ParseDiagnosticKind {
    #[error("Bad escaping!")]
    #[diagnostic(code(shebling::bad_escape), severity("warning"))]
    BadEscape,

    #[error("Unexpected character!")]
    #[diagnostic(code(shebling::unexpected_char), severity("warning"))]
    UnexpectedChar,

    #[error("Unicode character!")]
    #[diagnostic(
        code(shebling::unichar),
        help("Delete and retype it."),
        severity("warning")
    )]
    Unichar,
}

#[derive(Debug, Diagnostic, Error)]
#[error("Parser bailed!")]
#[diagnostic(code("shebling::parser_error"))]
struct ParseError {
    #[label("stopped here")]
    location: Location,
    #[related]
    notes: Vec<LabeledRange>,
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
        other.notes.push(LabeledRange {
            range: Range::from(input),
            label: ctx,
        });

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
