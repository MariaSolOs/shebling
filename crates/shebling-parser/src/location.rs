use std::borrow::Borrow;

use crate::ParseContext;

pub(crate) type Span<'a> = nom_locate::LocatedSpan<&'a str, ParseContext>;

#[derive(Clone, Copy, Debug)]
pub(crate) struct Location {
    offset: usize,
    line: u32,
    column: usize,
}

impl Location {
    /// Returns the location's line.
    pub(crate) fn line(&self) -> u32 {
        self.line
    }

    /// Returns the location's column.
    pub(crate) fn column(&self) -> usize {
        self.column
    }

    /// Translates the location by the given offset.
    ///
    /// This method won't panic, since it saturates at the numeric bounds
    /// instead of overflowing.
    pub(crate) fn translate(mut self, offset: isize) -> Self {
        self.offset = self.offset.saturating_add_signed(offset);
        self.column = self.column.saturating_add_signed(offset);

        self
    }
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

#[derive(Clone, Copy, Debug)]
pub(crate) struct Range {
    start: Location,
    end: Location,
}

impl Range {
    /// Creates a new range from the given start and end locations.
    pub(crate) fn new(start: impl Into<Location>, end: impl Into<Location>) -> Self {
        Self {
            start: start.into(),
            end: end.into(),
        }
    }

    /// Returns the start location of the range.
    pub(crate) fn start(&self) -> &Location {
        &self.start
    }

    /// Returns the end location of the range.
    pub(crate) fn end(&self) -> &Location {
        &self.end
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
