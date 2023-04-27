use crate::{
    ast::Range,
    parser::{
        lint::{Lint, SUS_CHAR_AFTER_QUOTE, UNCLOSED_STRING},
        Span,
    },
};
use std::cell::RefCell;

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Context {
    lints: RefCell<Vec<Lint>>,
}

impl Context {
    pub(super) fn new() -> Self {
        Self {
            lints: RefCell::new(Vec::new()),
        }
    }

    pub(super) fn report(&self, lint: Lint) {
        self.lints.borrow_mut().push(lint);
    }

    // TODO: Move this fn to the parser.
    pub(super) fn report_unclosed_string(&self, start: &Span, end: &Span) {
        self.report(Lint::new(Range::new(start, end), UNCLOSED_STRING));
        self.report(Lint::new(Range::from(end), SUS_CHAR_AFTER_QUOTE));
    }

    pub(super) fn take_lints(&self) -> Vec<Lint> {
        self.lints.take()
    }

    pub(super) fn extend_lints(&self, other: Self) {
        self.lints.borrow_mut().extend(other.take_lints());
    }
}
