mod quoted;

use crate::{error::ParseError, span::ParseSpan};

use nom::{bytes::complete::take_till, character::complete::char, sequence::preceded};

pub(crate) type ParseResult<'a, R> = nom::IResult<ParseSpan<'a>, R, ParseError>;
