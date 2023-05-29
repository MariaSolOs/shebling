use std::fmt;

// region: Node location.
#[derive(Clone, Copy)]
pub struct Location {
    offset: usize,
    line: u32,
    column: usize,
}

impl Location {
    /// Returns the location's line.
    pub fn line(&self) -> u32 {
        self.line
    }

    /// Returns the location's column.
    pub fn column(&self) -> usize {
        self.column
    }
}

impl fmt::Debug for Location {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}, {})", self.line, self.column)
    }
}

impl From<Location> for miette::SourceSpan {
    fn from(value: Location) -> Self {
        value.offset.into()
    }
}

#[derive(Clone, Copy)]
pub struct Range {
    start: Location,
    end: Location,
}

impl Range {
    /// Creates a new range from the given start and end locations.
    pub fn new(start: impl Into<Location>, end: impl Into<Location>) -> Self {
        Self {
            start: start.into(),
            end: end.into(),
        }
    }
}

impl fmt::Debug for Range {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}..{:?}", self.start, self.end)
    }
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
