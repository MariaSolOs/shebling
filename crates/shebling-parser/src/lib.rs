// TODO: Remove all the #[allow(..)] after development.
#![allow(dead_code)]

mod ast;
mod diagnostic;
mod error;
mod location;
mod parser;

use std::cell::RefCell;

use diagnostic::{ParseDiagnostic, ParseDiagnosticBuilder};
use location::Span;

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
