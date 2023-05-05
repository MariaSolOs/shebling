use std::borrow::Borrow;

use crate::ParseContext;
use shebling_codegen::New;

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

#[derive(Clone, Copy, Debug, New)]
pub(crate) struct Range {
    #[new(into)]
    start: Location,
    #[new(into)]
    end: Location,
}

impl Range {
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
