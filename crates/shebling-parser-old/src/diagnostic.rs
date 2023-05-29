use std::fmt;
use thiserror::Error;

use crate::location::Range;

type LabeledRange = (String, Range);

#[derive(Clone, Debug, Error)]
#[error("{kind}")]
pub(crate) struct ParseDiagnostic {
    kind: ParseDiagnosticKind,
    labels: Vec<LabeledRange>,
    help: Option<String>,
}

// This implementation basically wraps the methods in ParseDiagnosticKind,
// but allowing us to keep the line and column information from the
// labeled ranges while still providing miette spans.
impl miette::Diagnostic for ParseDiagnostic {
    fn code<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        self.kind.code()
    }

    fn severity(&self) -> Option<miette::Severity> {
        Some(miette::Severity::Warning)
    }

    fn help<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        self.help
            .as_deref()
            .map(|help| Box::new(help) as Box<dyn fmt::Display>)
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = miette::LabeledSpan> + '_>> {
        Some(Box::new(self.labels.iter().map(|(label, range)| {
            miette::LabeledSpan::new_with_span(Some(label.into()), *range)
        })))
    }
}

impl ParseDiagnostic {
    /// Creates a new [builder](ParseDiagnosticBuilder) for a diagnostic
    /// of the given [kind](ParseDiagnosticKind).
    pub(crate) fn builder(kind: ParseDiagnosticKind) -> ParseDiagnosticBuilder {
        // Use the help from the kind, if it has one.
        let help = (&kind as &dyn miette::Diagnostic)
            .help()
            .map(|help| help.to_string());

        ParseDiagnosticBuilder {
            kind,
            labels: vec![],
            help,
        }
    }

    /// The [Range] of the first label, which is used as the diagnostic's
    /// main range.
    pub(crate) fn range(&self) -> &Range {
        &self
            .labels
            .first()
            .expect("Diagnostics should have at least one label.")
            .1
    }
}

pub(super) struct ParseDiagnosticBuilder {
    kind: ParseDiagnosticKind,
    labels: Vec<LabeledRange>,
    help: Option<String>,
}

impl ParseDiagnosticBuilder {
    /// Adds a [LabeledRange] to the [ParseDiagnostic].
    pub(crate) fn label(mut self, label: impl AsRef<str>, range: impl Into<Range>) -> Self {
        self.labels.push((label.as_ref().into(), range.into()));
        self
    }

    /// Adds a [LabeledRange] with an empty message to the [ParseDiagnostic].
    pub(crate) fn range(mut self, range: impl Into<Range>) -> Self {
        self.labels.push(("".into(), range.into()));
        self
    }

    /// Sets the help message of the [ParseDiagnostic].
    ///
    /// # Panics
    /// Panics if the diagnostic already has a help message.
    pub(crate) fn help(mut self, help: impl AsRef<str>) -> Self {
        if self.help.is_some() {
            panic!("This diagnostic already has a help message.");
        }

        self.help = Some(help.as_ref().into());
        self
    }

    /// Builds the [ParseDiagnostic].
    pub(super) fn build(self) -> ParseDiagnostic {
        ParseDiagnostic {
            kind: self.kind,
            labels: self.labels,
            help: self.help,
        }
    }
}

// TODO: Combine/separate diagnostics.
#[derive(Clone, Debug, Error, miette::Diagnostic)]
pub(crate) enum ParseDiagnosticKind {
    #[error("ambiguous command!")]
    #[diagnostic(code(shebling::ambiguous))]
    Ambiguous,

    #[error("bad escaping!")]
    #[diagnostic(code(shebling::bad_escape))]
    BadEscape,

    #[error("bad operator!")]
    #[diagnostic(code(shebling::bad_operator))]
    BadOperator,

    #[error("bad spacing!")]
    #[diagnostic(code(shebling::bad_space))]
    BadSpace,

    #[error("you should escape this character.")]
    #[diagnostic(code(shebling::missing_escape))]
    MissingEscape,

    #[error("you need a space here.")]
    #[diagnostic(code(shebling::missing_space))]
    MissingSpace,

    #[error("incorrect shell syntax!")]
    #[diagnostic(code(shebling::not_shell_code))]
    NotShellCode,

    #[error("this code looks a bit suspicious.")]
    #[diagnostic(code(shebling::sus_token))]
    SusToken,

    #[error("this assignment's value looks kinda sus.")]
    #[diagnostic(code(shebling::sus_value))]
    SusValue,

    #[error("you're missing some curlies here.")]
    #[diagnostic(code(shebling::unbraced))]
    Unbraced,

    #[error("unclosed string!")]
    #[diagnostic(code(shebling::unclosed_string))]
    UnclosedString,

    #[error("unicode character!")]
    #[diagnostic(
        code(shebling::unichar),
        help("Delete and retype it, or quote it if intended.")
    )]
    Unichar,
}
