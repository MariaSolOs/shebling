use std::{fmt, iter};

#[derive(Debug, thiserror::Error)]
#[error("{kind}")]
pub(crate) struct ParseDiagnostic {
    kind: ParseDiagnosticKind,
    offset: usize,
    label: &'static str,
}

impl ParseDiagnostic {
    pub(crate) fn new(kind: ParseDiagnosticKind, offset: usize, label: &'static str) -> Self {
        Self {
            kind,
            offset,
            label,
        }
    }
}

impl miette::Diagnostic for ParseDiagnostic {
    fn code<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        self.kind.code()
    }

    fn severity(&self) -> Option<miette::Severity> {
        Some(miette::Severity::Error)
    }

    fn help<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        self.kind.help()
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = miette::LabeledSpan> + '_>> {
        Some(Box::new(iter::once(miette::LabeledSpan::new_with_span(
            Some(self.label.into()),
            self.offset,
        ))))
    }
}

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
pub(crate) enum ParseDiagnosticKind {
    #[error("unclosed {0} string!")]
    #[diagnostic(code(shebling::unclosed_string))]
    UnclosedString(&'static str),
}
