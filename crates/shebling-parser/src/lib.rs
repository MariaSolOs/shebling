// TODO: Remove all the #[allow(..)] after development.
#![allow(dead_code)]

use shebling_codegen::New;
use std::{borrow::Borrow, cell::RefCell};

mod parser;

mod ast;

mod diagnostic;
use diagnostic::{ParseDiagnostic, ParseDiagnosticBuilder};

mod error;

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

// region: Parsing context.
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
// endregion

pub(crate) fn source_to_span(source_code: &str) -> Span {
    let context = ParseContext {
        diags: RefCell::new(Vec::new()),
    };

    nom_locate::LocatedSpan::new_extra(source_code, context)
}

#[allow(unused_variables)]
pub fn parse(file_path: impl AsRef<str>, source_code: &str) {
    // HACK: When reporting errors, add a newline to the end of the source
    // so that miette can highlight the last character.

    parser::test(file_path, source_code);
}
