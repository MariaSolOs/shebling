/// Range of an AST node in the source code. Represented by the start and end
/// byte offsets.
#[derive(Debug)]
pub struct Span(usize, usize);

impl Span {
    /// Creates a new span with the given start and end offsets.
    pub fn new(start: usize, end: usize) -> Self {
        Self(start, end)
    }
}

/// An AST node together with its [Span].
pub struct Spanned<T>(T, Span);

impl<T> Spanned<T> {
    /// Creates a new spanned object.
    pub fn new(t: T, span: Span) -> Self {
        Self(t, span)
    }
}

/// Trait for types that can be located in the source code.
pub trait Locate {
    /// Returns the start offset.
    fn start(&self) -> usize;

    /// Returns the end offset.
    fn end(&self) -> usize;
}

impl From<Span> for miette::SourceSpan {
    fn from(value: Span) -> Self {
        let start = value.0;
        let len = value.1 - start;

        (start, len).into()
    }
}

impl<T> From<Spanned<T>> for miette::SourceSpan {
    fn from(value: Spanned<T>) -> Self {
        value.1.into()
    }
}
