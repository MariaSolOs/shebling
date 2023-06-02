use crate::DiagnosticKind;

use std::fmt;

#[derive(Debug, thiserror::Error)]
#[error("{kind}")]
pub struct Diagnostic {
    kind: DiagnosticKind,
    labels: Vec<miette::LabeledSpan>,
    help: Option<String>,
}

impl Diagnostic {
    /// Creates a new [builder](DiagnosticBuilder) for a diagnostic
    /// of the given [kind](DiagnosticKind).
    pub fn builder(kind: DiagnosticKind) -> DiagnosticBuilder {
        // Use the help from the kind, if it has one.
        let help = (&kind as &dyn miette::Diagnostic)
            .help()
            .map(|help| help.to_string());

        DiagnosticBuilder {
            kind,
            labels: vec![],
            help,
        }
    }

    /// The [miette::SourceSpan] of the first label, which is used as the diagnostic's
    /// main span.
    pub fn span(&self) -> &miette::SourceSpan {
        &self
            .labels
            .first()
            .expect("Diagnostics should have at least one label.")
            .inner()
    }
}

// This implementation basically wraps the methods in DiagnosticKind,
// but allowing us to keep the location information in the wrapping
// Diagnostic and the metadata in DiagnosticKind.
impl miette::Diagnostic for Diagnostic {
    fn code<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        self.kind.code()
    }

    fn severity(&self) -> Option<miette::Severity> {
        self.kind.severity().or(Some(miette::Severity::Warning))
    }

    fn help<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        self.help
            .as_deref()
            .map(|help| Box::new(help) as Box<dyn fmt::Display>)
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = miette::LabeledSpan> + '_>> {
        Some(Box::new(self.labels.clone().into_iter()))
    }
}

pub struct DiagnosticBuilder {
    kind: DiagnosticKind,
    labels: Vec<miette::LabeledSpan>,
    help: Option<String>,
}

impl DiagnosticBuilder {
    /// Adds a labeled span to the [Diagnostic].
    pub fn label(mut self, label: impl AsRef<str>, span: impl Into<miette::SourceSpan>) -> Self {
        self.labels.push(miette::LabeledSpan::new_with_span(
            Some(label.as_ref().into()),
            span.into(),
        ));
        self
    }

    /// Sets the help message of the [Diagnostic].
    pub fn help(mut self, help: impl AsRef<str>) -> Self {
        self.help = Some(help.as_ref().into());
        self
    }

    /// Builds the [Diagnostic].
    pub fn build(self) -> Diagnostic {
        Diagnostic {
            kind: self.kind,
            labels: self.labels,
            help: self.help,
        }
    }
}
