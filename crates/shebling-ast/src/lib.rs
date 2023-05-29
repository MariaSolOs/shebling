use std::fmt;

// region: Node location.
/// Range of an AST node in the source code. Represented by the start and end
/// byte offsets.
pub struct Span {
    start: usize,
    end: usize,
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}..{}", self.start, self.end)
    }
}
// endregion
