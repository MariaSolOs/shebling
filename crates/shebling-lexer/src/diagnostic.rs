use std::fmt;

// TODO: Document.

#[derive(Debug, thiserror::Error)]
#[error("{kind}")]
pub struct LexerDiagnostic {
    kind: LexerDiagnosticKind,
    offset: usize,
    label: &'static str,
}

impl LexerDiagnostic {
    pub(crate) fn new(kind: LexerDiagnosticKind, offset: usize, label: &'static str) -> Self {
        Self {
            kind,
            offset,
            label,
        }
    }
}

impl miette::Diagnostic for LexerDiagnostic {
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
        Some(Box::new(
            vec![miette::LabeledSpan::new_with_span(
                Some(self.label.into()),
                self.offset,
            )]
            .into_iter(),
        ))
    }
}

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
pub enum LexerDiagnosticKind {
    #[error("CRLF line ending!")]
    #[diagnostic(
        code(shebling::cr_lf),
        help("Try running the script through tr -d '\\r'.")
    )]
    CrLf,

    #[error("unclosed {0}!")]
    #[diagnostic(code(shebling::unclosed_word))]
    UnclosedWord(&'static str),
}
