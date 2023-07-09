use nom::{bytes::complete::take, combinator::map, Offset, Slice};
use shebling_diagnostic::{Diagnostic, DiagnosticBuilder};
use std::{
    cell::RefCell,
    fmt,
    ops::{RangeFrom, RangeTo},
    str::{CharIndices, Chars},
};

use crate::parser::ParseResult;

// This module basically replicates what nom_locate does, except that it is a simpler
// version and we carry the diagnostic context in a separate reference instead of it
// being part of the span itself (which is lost on failure).

/// Diagnostics reported during parsing. These should serve as possible hints
/// of what went wrong in case of failure.
/// It might also contain minor lints that refer to code not included in the AST (e.g.
/// trivia).
#[derive(Debug)]
pub(crate) struct ParseDiags(RefCell<Vec<Diagnostic>>);

impl ParseDiags {
    pub(crate) fn new() -> Self {
        Self(RefCell::new(vec![]))
    }

    fn push(&self, builder: DiagnosticBuilder) {
        self.0.borrow_mut().push(builder.build());
    }
}

impl IntoIterator for ParseDiags {
    type Item = Diagnostic;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.take().into_iter()
    }
}

/// The input to `shebling` parsers.
/// Besides containing the input source, it also carries the diagnostic context.
#[derive(Clone, Debug)]
pub(crate) struct ParseSpan<'a> {
    fragment: &'a str,
    offset: usize,
    diags: &'a ParseDiags,
}

impl<'a> ParseSpan<'a> {
    pub(crate) fn new(source: &'a str, diags: &'a ParseDiags) -> Self {
        Self {
            fragment: source,
            offset: 0,
            diags,
        }
    }

    pub(crate) fn fragment(&self) -> &str {
        self.fragment
    }

    pub(crate) fn offset(&self) -> usize {
        self.offset
    }

    pub(crate) fn diag(&self, builder: DiagnosticBuilder) {
        self.diags.push(builder);
    }
}

impl fmt::Display for ParseSpan<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.fragment)
    }
}

impl nom::Offset for ParseSpan<'_> {
    fn offset(&self, second: &Self) -> usize {
        second.offset - self.offset
    }
}

impl<'a> nom::InputIter for ParseSpan<'a> {
    type Item = char;
    type Iter = CharIndices<'a>;
    type IterElem = Chars<'a>;

    #[inline]
    fn iter_indices(&self) -> Self::Iter {
        self.fragment.iter_indices()
    }

    #[inline]
    fn iter_elements(&self) -> Self::IterElem {
        self.fragment.iter_elements()
    }

    #[inline]
    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.fragment.position(predicate)
    }

    #[inline]
    fn slice_index(&self, count: usize) -> Result<usize, nom::Needed> {
        self.fragment.slice_index(count)
    }
}

impl<'a> nom::InputLength for ParseSpan<'a> {
    #[inline]
    fn input_len(&self) -> usize {
        self.fragment.input_len()
    }
}

impl<'a> nom::InputTake for ParseSpan<'a> {
    #[inline]
    fn take(&self, count: usize) -> Self {
        self.slice(..count)
    }

    #[inline]
    fn take_split(&self, count: usize) -> (Self, Self) {
        (self.slice(count..), self.slice(..count))
    }
}

impl nom::Slice<RangeFrom<usize>> for ParseSpan<'_> {
    fn slice(&self, range: RangeFrom<usize>) -> Self {
        let new_fragment = &self.fragment.slice(range);
        let new_offset = self.offset + self.fragment.offset(new_fragment);

        ParseSpan {
            fragment: new_fragment,
            offset: new_offset,
            diags: self.diags,
        }
    }
}

impl nom::Slice<RangeTo<usize>> for ParseSpan<'_> {
    fn slice(&self, range: RangeTo<usize>) -> Self {
        let new_fragment = &self.fragment.slice(range);
        let new_offset = self.offset + self.fragment.offset(new_fragment);

        ParseSpan {
            fragment: new_fragment,
            offset: new_offset,
            diags: self.diags,
        }
    }
}

impl<'a> nom::UnspecializedInput for ParseSpan<'a> {}

impl<'a> nom::Compare<&str> for ParseSpan<'a> {
    #[inline(always)]
    fn compare(&self, t: &str) -> nom::CompareResult {
        self.fragment.compare(t)
    }

    fn compare_no_case(&self, t: &str) -> nom::CompareResult {
        self.fragment.compare_no_case(t)
    }
}

/// Returns the offset of the given span.
pub(crate) fn offset(span: ParseSpan) -> ParseResult<usize> {
    map(take(0usize), |span: ParseSpan| span.offset())(span)
}
