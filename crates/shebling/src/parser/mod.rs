// TODO: Remove this.
#![allow(dead_code)]

mod context;
mod error;
mod lint;
#[cfg(test)]
mod tests;

use crate::{
    ast::*,
    parser::{context::Context, error::Error, lint::*},
    shell::COMMON_COMMANDS,
};
use nom::{
    branch::alt,
    bytes::complete::{is_a, is_not, tag, tag_no_case, take},
    character::complete::{alpha1, alphanumeric1, anychar, char, digit1, newline, one_of, satisfy},
    combinator::{
        all_consuming, consumed, cut, eof, fail, into, map, not, opt, peek, recognize, value,
        verify,
    },
    error::context,
    multi::{many0, many1, many_till, separated_list0, separated_list1},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    IResult,
};
use nom_locate::LocatedSpan;

pub(crate) type Span<'a> = LocatedSpan<&'a str, Context>;

pub(crate) type ParseResult<'a, R> = IResult<Span<'a>, R, Error>;

// region: Tokens.
/// Creates a parser for the given [ParseToken].
fn token<'a, P: ParseToken>(token: P) -> impl FnMut(Span<'a>) -> ParseResult<P> {
    move |span| {
        let (span, _) = token.parse_token(span)?;

        Ok((span, token))
    }
}

/// Creates a parser for the given token based on its `token()` pattern.
fn parse_token<'a, T: Token>(token: T) -> impl FnMut(Span<'a>) -> ParseResult<T> {
    value(token, tag(token.token()))
}

/// Trait for [Token] types which can be directly parsed from a [Span].
trait ParseToken
where
    Self: Token,
{
    fn parse_token<'a>(self, span: Span<'a>) -> ParseResult<'a, Self> {
        parse_token(self)(span)
    }
}

impl ParseToken for BinOp {}

impl ParseToken for UnOp {}

impl ParseToken for ClauseSep {
    fn parse_token<'a>(self, span: Span<'a>) -> ParseResult<'a, Self> {
        terminated(parse_token(self), trivia)(span)
    }
}

impl ParseToken for ControlOp {
    fn parse_token<'a>(self, mut span: Span<'a>) -> ParseResult<'a, Self> {
        // Special case for `;` since we don't want to mix it up with `;;`.
        if let ControlOp::Semi = self {
            (span, _) = not(token(ControlOp::DSemi))(span)?;
        }

        terminated(parse_token(self), trivia)(span)
    }
}

impl ParseToken for Keyword {
    fn parse_token<'a>(self, span: Span<'a>) -> ParseResult<'a, Self> {
        let keyword = self.token();

        let (span, word) = parsed(tag_no_case(keyword))(span)?;

        let span = if self == Keyword::Function {
            // Special case for `function`, since they can only be followed
            // (and have to) by horizontal whitespace.
            let (span, _) = many1(line_space)(span)?;

            span
        } else {
            // Keywords followed by '[', '#', '!', or ':' need a space between them.
            let (span, sus_char) = followed_by(one_of("[#!:"))(span)?;
            if sus_char {
                span.extra
                    .report(Lint::new(Range::from(&span), MISSING_SPACE));
            }

            // Reserved words must be followed by space or a metacharacter.
            let (span, _) =
                peek(alt((parsed(eof), multi_trivia1, parsed(one_of(";()<>&|")))))(span)?;

            span
        };

        if word != keyword {
            context(
                "Scripts are case-sensitive. Keywords should be lower-cased!",
                fail,
            )(span)
        } else {
            // Parse trailing trivia and return the keyword.
            value(self, trivia)(span)
        }
    }
}

impl ParseToken for RedirOp {
    fn parse_token<'a>(self, span: Span<'a>) -> ParseResult<'a, Self> {
        terminated(parse_token(self), trivia)(span)
    }
}
// endregion

const BRACED_ESCAPABLE: &str = "}\"$`'";
const DOUBLE_ESCAPABLE: &str = "\\\"$`";
const DOUBLE_UNIQUOTES: &str = "\u{201C}\u{201D}\u{2033}\u{2036}";
const EXTGLOB_PREFIX: &str = "?*@!+";
const SINGLE_UNIQUOTES: &str = "\u{2018}\u{2019}";
const SPECIAL_PARAMS: &str = "$?!#-@*";
const UNIDASHES: &str =
    "\u{058A}\u{05BE}\u{2010}\u{2011}\u{2012}\u{2013}\u{2014}\u{2015}\u{FE63}\u{FF0D}";
const UNISPACES: &str = "\u{A0}\u{200B}";

// region: Miscellaneous helper functions and parsers.
fn followed_by<'a, P, R>(parser: P) -> impl FnMut(Span<'a>) -> ParseResult<bool>
where
    P: FnMut(Span<'a>) -> ParseResult<R>,
{
    map(opt(peek(parser)), |res| res.is_some())
}

fn is_sus_char_after_quote(c: char) -> bool {
    c.is_ascii_alphanumeric() || matches!(c, '_' | '%')
}

fn lit_parser<'a, P, R>(parser: P) -> impl FnMut(Span<'a>) -> ParseResult<Lit>
where
    P: FnMut(Span<'a>) -> ParseResult<R>,
    R: Into<String>,
{
    map(parser, |res| Lit::new(res.into()))
}

fn parsed<'a, P, R>(parser: P) -> impl FnMut(Span<'a>) -> ParseResult<String>
where
    P: FnMut(Span<'a>) -> ParseResult<R>,
{
    map(recognize(parser), |span| (*span.fragment()).into())
}

fn ranged<'a, P, R>(parser: P) -> impl FnMut(Span<'a>) -> ParseResult<(R, Range)>
where
    P: FnMut(Span<'a>) -> ParseResult<R>,
{
    map(
        tuple((nom_locate::position, parser, nom_locate::position)),
        |(start, res, end)| (res, Range::new(start, end)),
    )
}

fn source_to_span(source: &str) -> Span {
    LocatedSpan::new_extra(source, Context::new())
}

fn swallow<'a, P, R>(parser: P) -> impl FnMut(Span<'a>) -> ParseResult<()>
where
    P: FnMut(Span<'a>) -> ParseResult<R>,
{
    value((), parser)
}
// endregion

// region: AST parsers.
fn arith_seq(span: Span) -> ParseResult<ArithSeq> {
    macro_rules! bin_op {
        ($op:path) => {
            ::nom::sequence::terminated(
                token($op),
                ::nom::combinator::not(::nom::character::complete::one_of("&|<>="))
            )
        };

        (($($op:path),+)) => { ::nom::branch::alt(($(bin_op!($op),)+)) };
    }

    fn addition(span: Span) -> ParseResult<ArithTerm> {
        chained(
            multiplication,
            alt((
                bin_op!(BinOp::Add),
                // Read binary minus, but check if it's a binary test operator.
                |span| {
                    let (span, (start_span, test_op)) = separated_pair(
                        nom_locate::position,
                        bin_op!(BinOp::Sub),
                        opt(peek(pair(
                            consumed(alt((
                                value(BinOp::EqEq, tag("eq")),
                                value(BinOp::Ge, tag("ge")),
                                value(BinOp::Gt, tag("gt")),
                                value(BinOp::Le, tag("le")),
                                value(BinOp::Lt, tag("lt")),
                                value(BinOp::Ne, tag("ne")),
                            ))),
                            nom_locate::position,
                        ))),
                    )(span)?;

                    if let Some(((test_op, math_op), end_span)) = test_op {
                        span.extra.report(Lint::with_message(
                            Range::new(start_span, end_span),
                            COMPARATOR_IN_MATH,
                            format_comparator_in_math(math_op.token(), &test_op),
                        ));
                    }

                    Ok((span, BinOp::Sub))
                },
            )),
        )(span)
    }

    fn and(span: Span) -> ParseResult<ArithTerm> {
        chained(bit_or, bin_op!(BinOp::And))(span)
    }

    fn arith_space(span: Span) -> ParseResult<()> {
        swallow(many0(alt((whitespace, preceded(backslash, newline)))))(span)
    }

    fn assignment(span: Span) -> ParseResult<ArithTerm> {
        map(
            pair(
                trinary,
                many0(separated_pair(
                    bin_op!((
                        BinOp::Eq,
                        BinOp::MultEq,
                        BinOp::DivEq,
                        BinOp::ModEq,
                        BinOp::AddEq,
                        BinOp::SubEq,
                        BinOp::ShlEq,
                        BinOp::ShrEq,
                        BinOp::BitAndEq,
                        BinOp::BitXorEq,
                        BinOp::BitOrEq
                    )),
                    arith_space,
                    trinary,
                )),
            ),
            |(head, mut tail)| {
                // This is basically a right fold (right-associative application of
                // the assignments).
                tail.reverse();
                if let Some((op, term)) = tail.into_iter().reduce(|(op, right), (next_op, left)| {
                    (next_op, ArithBinExpr::new(left, right, op).into())
                }) {
                    ArithBinExpr::new(head, term, op).into()
                } else {
                    head
                }
            },
        )(span)
    }

    fn bit_and(span: Span) -> ParseResult<ArithTerm> {
        chained(equated, bin_op!(BinOp::BitAnd))(span)
    }

    fn bit_or(span: Span) -> ParseResult<ArithTerm> {
        chained(bit_xor, bin_op!(BinOp::BitOr))(span)
    }

    fn bit_xor(span: Span) -> ParseResult<ArithTerm> {
        chained(bit_and, bin_op!(BinOp::BitXor))(span)
    }

    fn chained<'a, T, O>(term: T, op: O) -> impl FnMut(Span<'a>) -> ParseResult<ArithTerm>
    where
        T: Fn(Span) -> ParseResult<ArithTerm> + Copy,
        O: FnMut(Span<'a>) -> ParseResult<BinOp>,
    {
        // Parse >= 1 terms and then apply the operator in a left associative way.
        map(
            pair(term, many0(separated_pair(op, arith_space, term))),
            |(head, tail)| {
                tail.into_iter().fold(head, |acc, (op, term)| {
                    ArithBinExpr::new(acc, term, op).into()
                })
            },
        )
    }

    fn compared(span: Span) -> ParseResult<ArithTerm> {
        chained(shift, bin_op!((BinOp::Le, BinOp::Ge, BinOp::Lt, BinOp::Gt)))(span)
    }

    fn equated(span: Span) -> ParseResult<ArithTerm> {
        chained(compared, bin_op!((BinOp::EqEq, BinOp::Ne)))(span)
    }

    fn exponential(span: Span) -> ParseResult<ArithTerm> {
        chained(maybe_negated, bin_op!(BinOp::Pow))(span)
    }

    fn maybe_incremented_term(span: Span) -> ParseResult<ArithTerm> {
        // TODO: Use spans to distinguish prefix vs postfix incremented terms (if needed).
        alt((
            // Postfix incremented (e.g. x++).
            map(
                pair(
                    terminated(term, arith_space),
                    terminated(alt((token(UnOp::Inc), token(UnOp::Dec))), arith_space),
                ),
                |(expr, op)| ArithUnExpr::new(expr, op).into(),
            ),
            // Doesn't have a prefix or postfix increment.
            terminated(term, arith_space),
            // Prefix incremented (e.g. ++x).
            map(
                separated_pair(alt((token(UnOp::Inc), token(UnOp::Dec))), arith_space, term),
                |(op, expr)| ArithUnExpr::new(expr, op).into(),
            ),
        ))(span)
    }

    fn maybe_negated(span: Span) -> ParseResult<ArithTerm> {
        alt((
            into(negated),
            // Signed term (e.g. +x, -x).
            map(
                separated_pair(
                    alt((
                        // Make sure we don't match the inc/dec terms.
                        terminated(token(UnOp::Pos), not(token(UnOp::Pos))),
                        terminated(token(UnOp::Neg), not(token(UnOp::Neg))),
                    )),
                    arith_space,
                    maybe_incremented_term,
                ),
                |(op, expr)| ArithUnExpr::new(expr, op).into(),
            ),
            maybe_incremented_term,
        ))(span)
    }

    fn multiplication(span: Span) -> ParseResult<ArithTerm> {
        chained(exponential, bin_op!((BinOp::Mult, BinOp::Div, BinOp::Mod)))(span)
    }

    fn negated(span: Span) -> ParseResult<ArithUnExpr> {
        map(
            separated_pair(
                alt((token(UnOp::Not), token(UnOp::BitNeg))),
                arith_space,
                maybe_negated,
            ),
            |(op, expr)| ArithUnExpr::new(expr, op),
        )(span)
    }

    fn or(span: Span) -> ParseResult<ArithTerm> {
        chained(and, bin_op!(BinOp::Or))(span)
    }

    fn seq(span: Span) -> ParseResult<ArithSeq> {
        preceded(
            arith_space,
            map(
                separated_list0(pair(char(','), arith_space), assignment),
                ArithSeq::new,
            ),
        )(span)
    }

    fn shift(span: Span) -> ParseResult<ArithTerm> {
        chained(addition, bin_op!((BinOp::Shl, BinOp::Shr)))(span)
    }

    fn term(span: Span) -> ParseResult<ArithTerm> {
        terminated(
            alt((
                // A group.
                delimited(
                    char('('),
                    map(seq, |group| ArithGroup::new(group).into()),
                    char(')'),
                ),
                // A variable, which could be an array with more math stuff as an index.
                map(
                    pair(
                        identifier,
                        many0(delimited(
                            char('['),
                            // These will be parsed later based on the array being associative or not.
                            // TODO: Actually do that ^^
                            map(parsed(arith_seq), Subscript::new),
                            char(']'),
                        )),
                    ),
                    |(ident, indices)| Variable::with_subscripts(ident, indices).into(),
                ),
                map(
                    many1(alt((
                        into(single_quoted),
                        into(double_quoted),
                        unquoted_dollar_sgmt,
                        into(brace_expansion),
                        into(backquoted(false)),
                        into(lit_parser(char('#'))),
                        // Parse a literal until something that looks like a math operator.
                        lit_word_sgmt("+-*/=%^,]?:"),
                    ))),
                    |sgmts| ArithExpansion::new(sgmts).into(),
                ),
            )),
            arith_space,
        )(span)
    }

    fn trinary(span: Span) -> ParseResult<ArithTerm> {
        alt((
            map(
                tuple((
                    or,
                    preceded(pair(char('?'), arith_space), trinary),
                    preceded(pair(char(':'), arith_space), trinary),
                )),
                |(cond, then_branch, else_branch)| {
                    ArithTriExpr::new(cond, then_branch, else_branch).into()
                },
            ),
            or,
        ))(span)
    }

    seq(span)
}

fn assign(span: Span) -> ParseResult<Assign> {
    fn array(span: Span) -> ParseResult<Array> {
        // Read the opening brace, warning if it looks like a multi-D array.
        let (span, (first_paren, second_paren)) = terminated(
            pair(
                terminated(nom_locate::position, char('(')),
                opt(peek(preceded(char('('), nom_locate::position))),
            ),
            multi_trivia,
        )(span)?;
        if let Some(second_paren) = second_paren {
            span.extra
                .report(Lint::new(Range::new(first_paren, second_paren), NESTED_ARR));
        }

        // Parse the elements, which may be key-value pairs, arrays, or
        // regular words.
        let (span, elems) = many0(delimited(
            not(char(')')),
            alt((
                map(
                    separated_pair(
                        many1(subscript),
                        char('='),
                        alt((into(array), into(word), |span| Ok((span, Value::Empty)))),
                    ),
                    |(key, value)| KeyValue::new(key, value).into(),
                ),
                into(array),
                into(word),
            )),
            multi_trivia,
        ))(span)?;

        // Read the closing brace.
        let (span, _) = context("Expected a closing `)` for this array!", char(')'))(span)?;

        Ok((span, Array::new(elems)))
    }

    fn subscript(span: Span) -> ParseResult<Subscript> {
        delimited(
            char('['),
            map(
                parsed(many0(alt((
                    word_sgmt("]"),
                    into(lit_parser(trivia1)),
                    into(lit_parser(parsed(is_a(&*format!(
                        "|&;<>() '\t\n\r\u{A0}{}",
                        DOUBLE_ESCAPABLE
                    ))))),
                )))),
                Subscript::new,
            ),
            char(']'),
        )(span)
    }

    // Parse the variable name and any array indices.
    let (span, (ident, subscripts)) = preceded(
        context(
            "Don't use `$` on the left side of assignments!",
            not(char('$')),
        ),
        pair(context("Invalid identifier!", identifier), many0(subscript)),
    )(span)?;

    // Parse the assignment operator.
    let (span, (start_op, op)) = pair(
        nom_locate::position,
        context(
            "Expected an assignment operator!",
            alt((token(BinOp::AddEq), token(BinOp::Eq))),
        ),
    )(span)?;

    // Check for `==`.
    let (span, trailing_op_end) =
        opt(peek(preceded(token(BinOp::Eq), nom_locate::position)))(span)?;

    // Read any spaces after the operator.
    let (span, ((right_space, space_range), end_of_cmd)) = pair(
        ranged(map(trivia, |trivia| !trivia.is_empty())),
        followed_by(alt((is_a("\r\n;&|)"), eof))),
    )(span)?;

    let (span, value) = if right_space || end_of_cmd {
        // If there's a space after the operator, the value will be an empty string.
        // We don't warn for IFS because of the common `IFS= read ...` idiom.
        if ident != "IFS" && right_space && !end_of_cmd {
            span.extra.report(Lint::new(space_range, SPACE_AFTER_EQ));
        }

        (span, Value::Empty)
    } else {
        // Warn about `==` because the 2nd `=` will be part of the value.
        if let Some(trailing_op_end) = trailing_op_end {
            span.extra.report(Lint::new(
                Range::new(start_op, trailing_op_end),
                MISUSED_EQEQ,
            ));
        }

        // Read the assignment value.
        terminated(alt((into(array), into(word))), trivia)(span)?
    };

    Ok((
        span,
        Assign::new(Variable::with_subscripts(ident, subscripts), value, op),
    ))
}

fn backquoted(escape_double_quotes: bool) -> impl Fn(Span) -> ParseResult<BackQuoted> {
    fn backquote(span: Span) -> ParseResult<Span> {
        alt((tag("`"), |span| {
            let (span, (tick, range)) = ranged(tag("´"))(span)?;
            span.extra.report(Lint::new(range, FORWARD_TICKED));

            Ok((span, tick))
        }))(span)
    }

    move |span| {
        // Parse the quotes and the command string.
        let (span, (start_quote, cmd_start)) = pair(backquote, nom_locate::position)(span)?;
        let (span, cmd) = many0(alt((
            escaped(if escape_double_quotes {
                "$`\\\""
            } else {
                "$`\\"
            }),
            parsed(is_not("\\`´")),
        )))(span)?;
        let (span, end_quote) = backquote(span)?;
        let cmd = cmd.concat();

        // Does this look like an unclosed string?
        let (span, sus_next_char) = followed_by(satisfy(is_sus_char_after_quote))(span)?;
        if sus_next_char && !cmd.starts_with('\n') && cmd.contains('\n') {
            span.extra.report_unclosed_string(&start_quote, &end_quote);
        }

        // Subparse the quoted content.
        let cmd_span = unsafe {
            Span::new_from_raw_offset(
                cmd_start.location_offset(),
                cmd_start.location_line(),
                &cmd,
                Context::new(),
            )
        };
        let (cmd_span, cmd) = all_consuming(preceded(multi_trivia, term))(cmd_span)?;
        span.extra.extend_lints(cmd_span.extra);

        Ok((span, BackQuoted::new(cmd)))
    }
}

fn backslash(span: Span) -> ParseResult<char> {
    char('\\')(span)
}

fn bats_test(span: Span) -> ParseResult<BatsTest> {
    // Read the test header.
    let (span, mut header) =
        preceded(pair(tag("@test "), trivia), peek(parsed(is_not("\n"))))(span)?;

    // The test name is everything before the last " {".
    if let Some(header_len) = header.rfind(" {") {
        header.truncate(header_len);
    } else {
        return context("Invalid test name!", fail)(span);
    }

    // Parse the test body defined after the truncated header.
    let (span, body) = preceded(
        pair(take(header.len()), trivia),
        context("Invalid test body!", brace_group),
    )(span)?;

    Ok((span, BatsTest::new(header.trim_end(), body)))
}

fn brace_expansion(span: Span) -> ParseResult<BraceExpansion> {
    fn braced(span: Span) -> ParseResult<BraceExpansion> {
        delimited(
            char('{'),
            map(
                context(
                    "Invalid sequence expression!",
                    verify(
                        separated_list1(char(','), braced_word),
                        |words: &Vec<Word>| {
                            // If the brace expansion is a single word, it must be
                            // a lit of the form `x..y`.
                            if words.len() == 1 {
                                let word = &words[0];

                                if word.sgmts().is_empty() {
                                    return false;
                                } else if let Some(lit) = word.as_lit() {
                                    return lit.value().split_once("..").map_or(
                                        false,
                                        |(prefix, suffix)| {
                                            !prefix.is_empty()
                                                && !prefix.contains("..")
                                                && !suffix.is_empty()
                                                && !suffix.contains("..")
                                        },
                                    );
                                }
                            }

                            true
                        },
                    ),
                ),
                BraceExpansion::new,
            ),
            char('}'),
        )(span)
    }

    fn braced_word(span: Span) -> ParseResult<Word> {
        map(
            many0(alt((
                into(braced),
                into(dollar_exp),
                into(single_quoted),
                into(double_quoted),
                map(
                    many1(alt((
                        escaped(""),
                        parsed(is_not(&*format!("{{}}\"$', \t\r\n{}", UNISPACES))),
                    ))),
                    |lits| Lit::new(lits.concat()).into(),
                ),
            ))),
            Word::new,
        )(span)
    }

    braced(span)
}

fn brace_group(span: Span) -> ParseResult<Term> {
    // Check spacing after the opening brace, but note that `{(` is legal.
    let (span, (start_span, space, paren)) = tuple((
        terminated(nom_locate::position, char('{')),
        multi_trivia,
        followed_by(char('(')),
    ))(span)?;
    if space.is_empty() && !paren {
        span.extra
            .report(Lint::new(Range::from(&span), MISSING_SPACE));
    }

    // Check for empty groups.
    let (span, closing_brace) = opt(peek(preceded(char('}'), nom_locate::position)))(span)?;
    if let Some(end_span) = closing_brace {
        span.extra
            .report(Lint::new(Range::new(start_span, end_span), EMPTY_BLOCK));
    }

    // Parse the braced term.
    let (span, term) = term(span)?;

    // Check the closing brace.
    let (span, _) = terminated(
        context(
            "Missing right brace, or you might need a `;` or `\n` before it.",
            char('}'),
        ),
        trivia,
    )(span)?;

    Ok((span, term))
}

fn carriage_return(span: Span) -> ParseResult<char> {
    let (span, (cr, range)) = ranged(char('\r'))(span)?;
    span.extra.report(Lint::new(range, LITERAL_CR));

    Ok((span, cr))
}

fn case_clause(span: Span) -> ParseResult<CaseClause> {
    fn pattern_word(span: Span) -> ParseResult<Word> {
        let (span, (word, range)) = ranged(map(many1(word_sgmt("")), Word::new))(span)?;

        // Any literal except `esac` is valid pattern in a case statement.
        let esac = Keyword::Esac.token();
        if word.as_lit().is_some_and(|lit| lit.value() == esac) {
            span.extra.report(Lint::with_message(
                range,
                LITERAL_KEYWORD,
                format_literal_keyword(esac),
            ));
        }

        Ok((span, word))
    }

    fn sep(span: Span) -> ParseResult<Option<ClauseSep>> {
        alt((
            map(
                alt((
                    token(ClauseSep::Continue),
                    token(ClauseSep::Fallthrough),
                    token(ClauseSep::Break),
                )),
                Some,
            ),
            // The separator of the last clause can be omitted.
            map(peek(pair(linebreak, token(Keyword::Esac))), |_| None),
        ))(span)
    }

    // Stop if we peek an `esac`.
    let (span, _) = not(peek(token(Keyword::Esac)))(span)?;

    // Optional parenthesis:
    let (span, _) = pair(opt(char('(')), trivia)(span)?;

    // Parse the pattern.
    let (span, pattern) = separated_list1(
        terminated(char('|'), trivia),
        terminated(pattern_word, trivia),
    )(span)?;

    // Check the closing parenthesis.
    let (span, _) = pair(
        cut(context("Expected a closing `)`!", char(')'))),
        linebreak,
    )(span)?;

    // Parse the clause's body.
    let (span, cmd) = alt((map(peek(sep), |_| None), map(term, Some)))(span)?;

    // Read the separator.
    let (span, sep) = delimited(
        cut(context(
            "Did you forget the separator in the previous clause?",
            not(char(')')),
        )),
        sep,
        linebreak,
    )(span)?;

    Ok((span, CaseClause::new(pattern, cmd, sep)))
}

fn case_cmd(span: Span) -> ParseResult<CaseCmd> {
    terminated(
        map(
            pair(
                // The header:
                delimited(
                    token(Keyword::Case),
                    word,
                    pair(
                        multi_trivia,
                        cut(context("Expected `in`!", token(Keyword::In))),
                    ),
                ),
                // The clauses:
                preceded(linebreak, many0(case_clause)),
            ),
            |(word, clauses)| CaseCmd::new(word, clauses),
        ),
        cut(context("Expected a closing `esac`!", token(Keyword::Esac))),
    )(span)
}

fn cmd(span: Span) -> ParseResult<Cmd> {
    alt((
        into(compound_cmd),
        into(cond_cmd),
        into(coproc),
        into(simple_cmd),
    ))(span)
}

fn comment(span: Span) -> ParseResult<String> {
    parsed(pair(char('#'), is_not("\r\n")))(span)
}

fn compound_cmd(span: Span) -> ParseResult<CompoundCmd> {
    // Read the command and suffix redirections.
    let (span, (cmd, redirs)) = pair(
        alt((
            map(brace_group, Construct::BraceGroup),
            // TODO: readAmbiguous "((" readArithmeticExpression readSubshell and consider error 1105
            map(subshell, Construct::Subshell),
            map(cond_block(Keyword::While), Construct::While),
            map(cond_block(Keyword::Until), Construct::Until),
            map(if_cmd, Construct::If),
            map(for_loop, Construct::ForLoop),
            preceded(token(Keyword::Select), map(in_listed, Construct::Select)),
            map(case_cmd, Construct::Case),
            map(bats_test, Construct::BatsTest),
            map(function, Construct::Function),
        )),
        many0(redir),
    )(span)?;

    // Check for post-command gibberish.
    let (span, trailing_word) = opt(peek(preceded(
        not(alt((peek_sus_token, swallow(peek(char('{')))))),
        ranged(many1(word)),
    )))(span)?;
    if let Some((_, range)) = trailing_word {
        span.extra.report(Lint::new(range, TRAILING_POST_CMD));
    }

    Ok((span, CompoundCmd::new(cmd, redirs)))
}

fn cond(span: Span) -> ParseResult<Cond> {
    fn and<'a>(single_bracketed: bool) -> impl FnMut(Span<'a>) -> ParseResult<CondExpr> {
        chained(
            move |span| maybe_negated_expr(single_bracketed)(span),
            logic_op(single_bracketed, BinOp::And, "-a"),
        )
    }

    fn bin_expr<'a>(single_bracketed: bool) -> impl Fn(Span<'a>) -> ParseResult<CondBinExpr> {
        move |span| {
            // Read the first argument.
            let (span, left) = cond_word(span)?;

            // Parse the operator.
            let (span, op) = delimited(
                pair(trivia, peek_arith),
                map(
                    escaped_or_quoted(alt((
                        verify(flag_op, |cond_op| cond_op != "-o" && cond_op != "-a"),
                        into(alt((
                            token(BinOp::EqEq),
                            token(BinOp::Ne),
                            token(BinOp::Le),
                            token(BinOp::Ge),
                            token(BinOp::Match),
                            token(BinOp::Gt),
                            token(BinOp::Lt),
                            token(BinOp::Eq),
                        ))),
                    ))),
                    |(op, _)| op,
                ),
                cond_space(true, single_bracketed),
            )(span)?;

            // Read the second argument.
            let (span, right) = terminated(
                cut(context(
                    "Expected another argument for this binary expression!",
                    // The 2nd argument is parsed separately if the operator is `=~`.
                    |span| {
                        if op == BinOp::Match {
                            map(many1(regex_sgmts), |sgmts| {
                                let sgmts = sgmts.into_iter().flatten().collect::<Vec<_>>();
                                Word::new(sgmts).into()
                            })(span)
                        } else {
                            cond_word(span)
                        }
                    },
                )),
                trivia,
            )(span)?;

            Ok((span, CondBinExpr::new(left, right, op)))
        }
    }

    fn bin_or_nullary_expr<'a>(
        single_bracketed: bool,
    ) -> impl FnMut(Span<'a>) -> ParseResult<CondExpr> {
        move |span| {
            // Warn if the expression looks like a group.
            let (span, bracket) = opt(peek(ranged(char('['))))(span)?;
            if let Some((_, bracket_range)) = bracket {
                let (test, group) = if single_bracketed {
                    ("[ .. ]", "\\( .. \\)")
                } else {
                    ("[[ .. ]]", "( .. )")
                };
                span.extra.report(Lint::with_message(
                    bracket_range,
                    TEST_GROUP,
                    format_test_group(test, group),
                ));
            }

            alt((into(bin_expr(single_bracketed)), nullary_expr))(span)
        }
    }

    fn chained<'a, E, O>(expr: E, op: O) -> impl FnMut(Span<'a>) -> ParseResult<CondExpr>
    where
        E: Fn(Span) -> ParseResult<CondExpr> + Copy,
        O: FnMut(Span<'a>) -> ParseResult<CondOp>,
    {
        // Parse >= 1 expressions and then apply the operator in a left associative way.
        map(pair(expr, many0(pair(op, expr))), |(head, tail)| {
            tail.into_iter().fold(head, |acc, (op, expr)| {
                CondBinExpr::new(acc, expr, op).into()
            })
        })
    }

    fn cond_space(required: bool, single_bracketed: bool) -> impl Fn(Span) -> ParseResult<String> {
        move |span| {
            let (span, (space, range)) = ranged(multi_trivia)(span)?;

            if required && space.is_empty() {
                span.extra.report(Lint::new(range, MISSING_SPACE));
            }
            if single_bracketed && space.contains('\n') {
                span.extra.report(Lint::new(range, UNESCAPED_TEST_LF));
            }

            Ok((span, space))
        }
    }

    fn cond_word(span: Span) -> ParseResult<Word> {
        // Peek a misplaced bracket.
        let (span, _) = context("Unexpected bracket!", not(pair(trivia, char(']'))))(span)?;

        // Parse the word.
        let (span, word) = context("Invalid condition argument!", word)(span)?;

        // Check what the last character isn't closing the condition.
        if let WordSgmt::Lit(lit) = word
            .sgmts()
            .last()
            .expect("word() parses at least one segment.")
        {
            if lit.value().ends_with(']')
                && word.sgmts().iter().all(|sgmt| {
                    if let WordSgmt::Lit(lit) = sgmt {
                        !lit.value().contains('[')
                    } else {
                        true
                    }
                })
            {
                // The word doesn't contain a literal [, so it doesn't look like
                // an array or glob. Error!
                // return false;
                return context("Missing a space before this `]`.", fail)(span);
            }
        }

        Ok((span, word))
    }

    fn content<'a>(single_bracketed: bool) -> impl Fn(Span<'a>) -> ParseResult<CondExpr> {
        move |span| {
            // Check for if [] being used like if ().
            let (span, ident) = opt(peek(terminated(ranged(identifier), trivia1)))(span)?;
            if let Some((ident, range)) = ident {
                if COMMON_COMMANDS.contains(&*ident) {
                    span.extra.report(Lint::new(range, BRACKETED_IF));
                }
            }

            or(single_bracketed)(span)
        }
    }

    fn escaped_or_quoted<'a, P, R>(mut parser: P) -> impl FnMut(Span<'a>) -> ParseResult<(R, bool)>
    where
        P: FnMut(Span<'a>) -> ParseResult<R>,
    {
        move |span| {
            let (mut span, bs) = opt(backslash)(span)?;

            // If there's no backslash, check for quotes.
            let mut quote = None;
            if bs.is_none() {
                (span, quote) = opt(one_of("'\""))(span)?;
            }

            let (mut span, res) = parser(span)?;

            // Verify the closing quote.
            if let Some(quote) = quote {
                (span, _) = char(quote)(span)?
            }

            Ok((span, (res, bs.is_some() || quote.is_some())))
        }
    }

    fn flag_op(span: Span) -> ParseResult<CondOp> {
        // Read the dash, warning if it's a unidash.
        let (span, _) = alt((char('-'), |span| {
            // Try to parse an unidash.
            let (span, (unidash, range)) = ranged(one_of(UNIDASHES))(span)?;
            span.extra
                .report(Lint::with_message(range, UNICHAR, format_unichar("dash")));

            Ok((span, unidash))
        }))(span)?;

        // Parse the flag.
        let (span, flag) = cut(context("Expected a test operator!", alpha1))(span)?;

        Ok((span, CondOp::new(format!("-{}", flag))))
    }

    fn group<'a>(single_bracketed: bool) -> impl FnMut(Span<'a>) -> ParseResult<CondGroup> {
        let paren = |p| {
            move |span| {
                let (span, ((_, escaped), range)) = terminated(
                    ranged(escaped_or_quoted(char(p))),
                    cond_space(single_bracketed, single_bracketed),
                )(span)?;

                // Parentheses should be escaped inside `[..]`.
                if single_bracketed && !escaped {
                    span.extra.report(Lint::new(range, UNESCAPED_COND_GROUP));
                }

                // Parentheses should not be escaped inside `[[..]]`.
                if !single_bracketed && escaped {
                    span.extra
                        .report(Lint::new(range, UNNECESSARY_COND_GROUP_ESCAPE));
                }

                Ok((span, ()))
            }
        };

        delimited(
            paren('('),
            map(or(single_bracketed), CondGroup::new),
            paren(')'),
        )
    }

    fn logic_op<'a, O: ParseToken>(
        single_bracketed: bool,
        unflagged_op: O,
        flagged_op: &'static str,
    ) -> impl FnMut(Span<'a>) -> ParseResult<CondOp> {
        alt((
            // Unflagged logic operator (e.g. `&&`).
            terminated(
                map(token(unflagged_op), |op| CondOp::new(op.token())),
                cond_space(false, single_bracketed),
            ),
            // Flagged logic operator (e.g. `-a`).
            terminated(
                verify(flag_op, move |op| op == flagged_op),
                cond_space(true, single_bracketed),
            ),
        ))
    }

    fn maybe_negated_expr<'a>(
        single_bracketed: bool,
    ) -> impl FnMut(Span<'a>) -> ParseResult<CondExpr> {
        let expr = move |span| {
            alt((
                into(group(single_bracketed)),
                into(unary_expr(single_bracketed)),
                bin_or_nullary_expr(single_bracketed),
            ))(span)
        };

        // Read an expression, which could be negated.
        terminated(
            alt((
                map(
                    separated_pair(token(UnOp::Not), cond_space(true, single_bracketed), expr),
                    |(op, expr)| CondUnExpr::new(expr, op).into(),
                ),
                expr,
            )),
            cond_space(false, single_bracketed),
        )
    }

    fn nullary_expr(span: Span) -> ParseResult<CondExpr> {
        // Parse the expression.
        let (span, (expr, expr_end)) = terminated(
            pair(cond_word, nom_locate::position),
            pair(trivia, peek_arith),
        )(span)?;

        // Check if the expression ends with what looks like a binary operator. If
        // so, the user might have forgotten a space.
        if let Some(WordSgmt::Lit(lit)) = expr.sgmts().last() {
            let lit = lit.value();
            if let Some(op) = vec![
                "-nt", "-ot", "-ef", "-eq", "-ne", "-lt", "-le", "-gt", "-ge", "==", "!=", "=~",
                ">", "<", "<=", ">=", "=",
            ]
            .into_iter()
            .find_map(|op| {
                // Consider both escaped and unescaped operators.
                let escaped = format!("\\{}", op);
                if lit.ends_with(&escaped) {
                    Some(escaped)
                } else if lit.ends_with(op) {
                    Some(op.into())
                } else {
                    None
                }
            }) {
                let op_end = Location::from(expr_end);
                let op_start = Location::new(*op_end.line(), op_end.column() - op.len());
                span.extra
                    .report(Lint::new(Range::from(op_start), MISSING_SPACE));
            }
        }

        Ok((span, expr.into()))
    }

    fn or<'a>(single_bracketed: bool) -> impl FnMut(Span<'a>) -> ParseResult<CondExpr> {
        chained(
            move |span| and(single_bracketed)(span),
            preceded(peek_arith, logic_op(single_bracketed, BinOp::Or, "-o")),
        )
    }

    fn peek_arith(span: Span) -> ParseResult<()> {
        // Peek a mathy operator character.
        let (span, math_following) = opt(peek(pair(
            nom_locate::position,
            alt((
                preceded(one_of("+*/%"), nom_locate::position),
                delimited(char('-'), nom_locate::position, char(' ')),
            )),
        )))(span)?;

        // Tests should do math stuff inside $((..)).
        if let Some((start, end)) = math_following {
            span.extra
                .report(Lint::new(Range::new(start, end), MATH_IN_TEST));
        }

        Ok((span, ()))
    }

    fn regex_sgmts(span: Span) -> ParseResult<Vec<WordSgmt>> {
        alt((
            // Read a group.
            map(
                tuple((
                    into(lit_parser(char('('))),
                    many0(alt((
                        regex_sgmts,
                        map(
                            many1(alt((
                                escaped(""),
                                parsed(is_not(&*format!("'(){}", DOUBLE_ESCAPABLE))),
                            ))),
                            |lits| vec![Lit::new(lits.concat()).into()],
                        ),
                    ))),
                    into(lit_parser(char(')'))),
                )),
                |(left, sgmts, right)| {
                    let mut sgmts = sgmts.into_iter().flatten().collect::<Vec<_>>();
                    // Push the group's parens.
                    sgmts.insert(0, left);
                    sgmts.push(right);

                    sgmts
                },
            ),
            // Ungrouped regex segment.
            map(
                alt((
                    into(single_quoted),
                    into(double_quoted),
                    into(dollar_exp),
                    lit_word_sgmt("( "),
                    into(lit_parser(one_of(&*format!("{}{{}}[]|$", EXTGLOB_PREFIX)))),
                )),
                |sgmt| vec![sgmt],
            ),
        ))(span)
    }

    fn unary_expr<'a>(single_bracketed: bool) -> impl FnMut(Span<'a>) -> ParseResult<CondUnExpr> {
        map(
            separated_pair(
                flag_op,
                cond_space(true, single_bracketed),
                cut(context(
                    "Expected an operand for the unary expression!",
                    terminated(cond_word, trivia),
                )),
            ),
            |(op, expr)| CondUnExpr::new(expr, op),
        )
    }

    // Parse the opening bracket.
    let (span, second_bracket) = preceded(char('['), opt(char('[')))(span)?;
    let single_bracketed = second_bracket.is_none();

    // Check spacing.
    let (span, space) = cond_space(true, single_bracketed)(span)?;

    // Get the content (if it isn't an empty condition).
    let (span, bracket_following) = followed_by(char(']'))(span)?;
    let (span, cond) = if bracket_following && !space.is_empty() {
        (span, Cond::new(single_bracketed))
    } else {
        map(content(single_bracketed), |expr| {
            Cond::with_expr(single_bracketed, expr)
        })(span)?
    };

    // Verify the closing bracket.
    let (span, (second_bracket, bracket_range)) = terminated(
        context(
            "Expected the test to end here (don't wrap commands in square brackets!).",
            ranged(preceded(char(']'), opt(char(']')))),
        ),
        trivia,
    )(span)?;

    // Make sure the brackets match.
    if (single_bracketed && second_bracket.is_some())
        || (!single_bracketed && second_bracket.is_none())
    {
        span.extra
            .report(Lint::new(bracket_range, COND_BRACKET_MISMATCH));
    }

    Ok((span, cond))
}

fn cond_block<'a>(keyword: Keyword) -> impl FnMut(Span<'a>) -> ParseResult<CondBlock> {
    preceded(
        token(keyword),
        map(pair(term, do_group), |(cond, block)| {
            CondBlock::new(cond, block)
        }),
    )
}

fn cond_cmd(span: Span) -> ParseResult<CondCmd> {
    // Read the condition and suffix redirections.
    let (span, (cond, redirs)) = pair(cond, many0(redir))(span)?;

    // `||` or `&&` should be used between test conditions.
    let (span, cond_op) = opt(peek(ranged(alt((
        preceded(char('-'), alt((tag("o"), tag("a")))),
        tag("or"),
        tag("and"),
    )))))(span)?;
    if let Some((_, range)) = cond_op {
        span.extra.report(Lint::new(range, OUTER_FLAG_OP));
    }

    // Check if there's a trailing word that's not keyword/operator-like.
    let (span, (sus_token, next_word)) = pair(opt(peek_sus_token), opt(peek(ranged(word))))(span)?;
    if let Some((_, range)) = next_word {
        if sus_token.is_none() && cond_op.is_none() {
            span.extra.report(Lint::new(range, SUS_POST_COND));
        }
    }

    Ok((span, CondCmd::new(cond, redirs)))
}

fn coproc(span: Span) -> ParseResult<Coproc> {
    preceded(
        pair(token(Keyword::Coproc), whitespace),
        alt((
            map(
                pair(opt(terminated(identifier, whitespace)), compound_cmd),
                |(name, cmd)| Coproc::new(name, cmd),
            ),
            map(simple_cmd, |cmd| Coproc::new(None, cmd)),
        )),
    )(span)
}

fn dollar_cmd_expansion(span: Span) -> ParseResult<DollarCmdExpansion> {
    delimited(
        tuple((tag("${"), whitespace, multi_trivia)),
        map(term, DollarCmdExpansion::new),
        context("Expected a `}`!", char('}')),
    )(span)
}

fn dollar_cmd_sub(span: Span) -> ParseResult<DollarCmdSub> {
    delimited(
        pair(tag("$("), multi_trivia),
        map(
            alt((
                // Parse the substitution, which can be empty.
                map(peek(alt((char(')'), value(char::default(), eof)))), |_| {
                    None
                }),
                map(term, Some),
            )),
            DollarCmdSub::new,
        ),
        context("Expected a closing `)`!", char(')')),
    )(span)
}

fn dollar_exp(span: Span) -> ParseResult<DollarExp> {
    preceded(
        // Make sure we don't try all these if there's no $.
        peek(char('$')),
        alt((
            // Math in $(()).
            delimited(
                tag("$(("),
                into(arith_seq),
                pair(
                    char(')'),
                    cut(context(
                        "Expected a double `))` to end the `$((..))`.",
                        char(')'),
                    )),
                ),
            ),
            into(dollar_cmd_sub),
            // TODO: Consider error SC1102.
            // Math in $[].
            delimited(pair(char('$'), char('[')), into(arith_seq), char(']')),
            into(dollar_cmd_expansion),
            into(param_expansion),
            into(dollar_variable),
        )),
    )(span)
}

fn dollar_variable(span: Span) -> ParseResult<Variable> {
    preceded(
        char('$'),
        map(
            alt((
                // A numerical variable (e.g.: `$0`).
                |span| {
                    let (span, ((dig, multi_digit), range)) =
                        ranged(pair(satisfy(|c| c.is_ascii_digit()), followed_by(digit1)))(span)?;
                    if multi_digit {
                        span.extra.report(Lint::new(range, UNBRACED_POSITIONAL));
                    }

                    Ok((span, dig.into()))
                },
                // Special parameters.
                into(one_of::<_, _, Error>(SPECIAL_PARAMS)),
                // A normal variable.
                |span| {
                    let (span, ((ident, has_bracket), range)) =
                        ranged(pair(identifier, followed_by(char('['))))(span)?;
                    if has_bracket {
                        span.extra.report(Lint::new(range, UNBRACED_INDEX));
                    }

                    Ok((span, ident))
                },
            )),
            |ident| Variable::new(ident).into(),
        ),
    )(span)
}

fn double_quoted(span: Span) -> ParseResult<DoubleQuoted> {
    fn double_quoted_lit(span: Span) -> ParseResult<DoubleQuotedSgmt> {
        map(
            many1(alt((
                // Escape sequence.
                |span| {
                    let (span, (escaped, range)) = ranged(escaped(DOUBLE_ESCAPABLE))(span)?;

                    if escaped.len() >= 2 {
                        // Some non-special character was escaped.
                        span.extra.report(Lint::new(range, UNSPECIAL_ESCAPE));
                    }

                    Ok((span, escaped))
                },
                // Parse until we hit an escapable char or a unicode quote.
                parsed(is_not(&*format!(
                    "{}{}",
                    DOUBLE_ESCAPABLE, DOUBLE_UNIQUOTES
                ))),
            ))),
            |sgmt| Lit::new(sgmt.concat()).into(),
        )(span)
    }

    fn lonely_dollar(span: Span) -> ParseResult<DoubleQuotedSgmt> {
        // Parse the literal dollar.
        let (span, (dollar, range)) = ranged(into(lit_parser(char('$'))))(span)?;

        // Sometimes people want to escape a dollar inside double quotes and end the string
        // to achieve that. But that's ugly, they should just use a backslash.
        let (span, should_escape) = followed_by(tuple((
            char('"'),
            opt(char('"')),
            alt((satisfy(|c| c.is_ascii_alphanumeric()), one_of("_?!#-@"))),
        )))(span)?;
        if should_escape {
            span.extra.report(Lint::new(range, UNESCAPED_DOLLAR));
        }

        Ok((span, dollar))
    }

    // Parse the quotes and string segments.
    let (span, start) = tag("\"")(span)?;
    let (span, sgmts) = many0(alt((
        double_quoted_lit,
        into(dollar_exp),
        lonely_dollar,
        into(backquoted(true)),
        into(lit_parser(double_uniquote)),
    )))(span)?;
    let (span, end) = cut(context("Expected end of double quoted string!", tag("\"")))(span)?;

    // Does this look like an unclosed string?
    let (span, sus_next_char) =
        followed_by(alt((satisfy(is_sus_char_after_quote), one_of("$\""))))(span)?;
    if sus_next_char {
        if let Some(DoubleQuotedSgmt::Lit(first_lit)) = sgmts.first() {
            if !first_lit.value().starts_with('\n')
                && sgmts.iter().any(|sgmt| {
                    if let DoubleQuotedSgmt::Lit(lit) = sgmt {
                        lit.value().contains('\n')
                    } else {
                        false
                    }
                })
            {
                span.extra.report_unclosed_string(&start, &end);
            }
        }
    }

    Ok((span, DoubleQuoted::new(sgmts)))
}

fn double_uniquote(span: Span) -> ParseResult<char> {
    let (span, (quote, range)) = ranged(one_of(DOUBLE_UNIQUOTES))(span)?;
    span.extra.report(Lint::with_message(
        range,
        UNICHAR,
        format_unichar("double quote"),
    ));

    Ok((span, quote))
}

fn do_group(span: Span) -> ParseResult<Term> {
    // `do` should come before the `done`.
    let (span, _) = context(
        "Did you forget the `do` for this loop?",
        not(token(Keyword::Done)),
    )(span)?;

    // Parse the `do`.
    let (span, _) = context("Expected `do`!", token(Keyword::Do))(span)?;

    // Warn about `do;`s.
    let (span, semi) = terminated(opt(ranged(token(ControlOp::Semi))), multi_trivia)(span)?;
    if let Some((_, range)) = semi {
        span.extra.report(Lint::with_message(
            range,
            DISALLOWED_SEMI,
            format_disallowed_semi(Keyword::Do.token()),
        ));
    }

    // Parse the term, bailing if the clause is empty.
    let (span, term) = preceded(
        context("Empty `do` clause!", not(token(Keyword::Done))),
        term,
    )(span)?;

    // Parse the `done`.
    // TODO: Create a subdiagnostic indicating the unmatched `do`.
    let (span, _) = context("Expected `done`!", token(Keyword::Done))(span)?;

    // If we peek the beginning of a process substitution, the user might have missed a '<'.
    let (span, proc_sub_start) = opt(peek(ranged(tag("<("))))(span)?;
    if let Some((_, range)) = proc_sub_start {
        span.extra.report(Lint::new(range, DONE_PROC_SUB));
    }

    Ok((span, term))
}

fn bracketed_glob(span: Span) -> ParseResult<Glob> {
    map(
        delimited(
            char('['),
            pair(
                opt(one_of("!^")),
                alt((
                    map(char(']'), |c| vec![c.into()]),
                    many1(alt((
                        // A POSIX character class.
                        parsed(delimited(tag("[:"), alpha1, tag(":]"))),
                        // Some other literal sequence.
                        map(lit_sgmt("]"), |lit| lit.value().into()),
                        // A globby character.
                        map(alt((char('['), one_of(EXTGLOB_PREFIX))), |c| c.into()),
                    ))),
                )),
            ),
            char(']'),
        ),
        |(neg_prefix, mut sgmts)| {
            if let Some(negation) = neg_prefix {
                // Add the negation prefix to the beginning of the glob.
                sgmts.insert(0, negation.into());
            }

            Glob::new(format!("[{}]", sgmts.concat()))
        },
    )(span)
}

fn escaped<'a>(can_escape: &'static str) -> impl FnMut(Span<'a>) -> ParseResult<String> {
    alt((
        map(pair(backslash, newline), |_| String::new()),
        map(pair(backslash, anychar), move |(bs, c)| {
            if can_escape.contains(c) {
                c.into()
            } else {
                format!("{}{}", bs, c)
            }
        }),
    ))
}

fn extglob(span: Span) -> ParseResult<Glob> {
    fn group(span: Span) -> ParseResult<String> {
        parsed(delimited(char('('), sgmt, char(')')))(span)
    }

    fn sgmt(span: Span) -> ParseResult<String> {
        parsed(separated_list0(
            char('|'),
            parsed(many0(alt((
                group,
                parsed(word_sgmt("")),
                parsed(many1(whitespace)),
                parsed(is_a("<>#;&")),
            )))),
        ))(span)
    }

    map(parsed(pair(one_of(EXTGLOB_PREFIX), group)), Glob::new)(span)
}

fn for_loop(span: Span) -> ParseResult<ForLoop> {
    fn arith_for_loop(span: Span) -> ParseResult<ArithForLoop> {
        // Parse the loop's header.
        let (span, header) = delimited(
            arith_parens(
                '(',
                "Missing the second `(` opening this arithmetic for-loop!",
            ),
            tuple((
                terminated(arith_seq, pair(char(';'), trivia)),
                terminated(arith_seq, pair(char(';'), trivia)),
                terminated(arith_seq, trivia),
            )),
            arith_parens(
                ')',
                "Missing the second `)` closing this arithmetic for-loop!",
            ),
        )(span)?;

        // There can be an optional separator before the `do`.
        let (span, _) = preceded(
            trivia,
            opt(terminated(
                alt((preceded(token(ControlOp::Semi), linebreak), newline_list)),
                trivia,
            )),
        )(span)?;

        // Parse the body.
        let (span, body) = alt((brace_group, do_group))(span)?;

        Ok((span, ArithForLoop::new(header, body)))
    }

    fn arith_parens(paren: char, ctx_msg: &'static str) -> impl Fn(Span) -> ParseResult<()> {
        move |span| {
            // Parse both parentheses.
            let (span, trivia) = delimited(
                char(paren),
                opt(ranged(trivia1)),
                context(ctx_msg, char(paren)),
            )(span)?;

            // Can't have space in-between.
            if let Some((_, range)) = trivia {
                span.extra.report(Lint::with_message(
                    range,
                    SPACED_FOR_PAREN,
                    format_spaced_for_paren(paren),
                ));
            }

            Ok((span, ()))
        }
    }

    preceded(
        token(Keyword::For),
        alt((into(arith_for_loop), into(in_listed))),
    )(span)
}

fn function(span: Span) -> ParseResult<Function> {
    fn parens(span: Span) -> ParseResult<()> {
        swallow(separated_pair(
            char('('),
            trivia,
            alt((char(')'), |span| {
                // No need for parameter lists.
                let (span, (_, range)) = ranged(is_not("\n){"))(span)?;
                span.extra.report(Lint::new(range, PARAM_LIST));

                char(')')(span)
            })),
        ))(span)
    }

    fn with_keyword(span: Span) -> ParseResult<String> {
        // Read `function` followed by the name.
        let (span, name) = preceded(
            token(Keyword::Function),
            parsed(many1(alt((
                satisfy(|c: char| c.is_ascii_alphanumeric()),
                one_of("_:+?-./^@,[]*=!"),
            )))),
        )(span)?;

        let (span, (trivia, parens, body_follows)) =
            tuple((trivia, opt(parens), followed_by(one_of("{("))))(span)?;

        // Without parentheses, we need space before the function's body.
        if trivia.is_empty() && parens.is_none() && body_follows {
            // TODO: Subdiagnostic saying "You need a space or linefeed between the function name and body."
            span.extra
                .report(Lint::new(Range::from(&span), MISSING_SPACE));
        }

        Ok((span, name))
    }

    fn without_keyword(span: Span) -> ParseResult<String> {
        // Read the name, making sure it isn't `time`.
        let (span, name) = verify(
            parsed(many1(alt((
                satisfy(|c: char| c.is_ascii_alphanumeric()),
                one_of("_:+?-./^@,"),
            )))),
            |name: &String| name != "time",
        )(span)?;

        let (span, _) = pair(trivia, parens)(span)?;

        Ok((span, name))
    }

    map(
        separated_pair(
            alt((with_keyword, without_keyword)),
            pair(
                multi_trivia,
                peek(context(
                    "Expected a `{` or `(` opening the function's body!",
                    one_of("{("),
                )),
            ),
            alt((brace_group, subshell)),
        ),
        |(name, body)| Function::new(name, body),
    )(span)
}

fn identifier(span: Span) -> ParseResult<String> {
    parsed(pair(
        // Make sure that the first character is not a number.
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(span)
}

fn if_cmd(span: Span) -> ParseResult<IfCmd> {
    fn else_branch(span: Span) -> ParseResult<Term> {
        // `else if` is not the same as `elif`. Lint that.
        let (span, (start, else_if_end)) = pair(
            nom_locate::position,
            delimited(
                token(Keyword::Else),
                opt(peek(preceded(token(Keyword::If), nom_locate::position))),
                semicolon_check(Keyword::Else),
            ),
        )(span)?;
        if let Some(end) = else_if_end {
            span.extra
                .report(Lint::new(Range::new(start, end), ELIF_LIKE));
        }

        // Parse the branch's term, bailing if it's empty.
        alt((
            term,
            cut(preceded(
                peek(alt((
                    token(Keyword::Fi),
                    token(Keyword::Elif),
                    token(Keyword::Else),
                ))),
                context("Empty `else` clause!", fail),
            )),
        ))(span)
    }

    fn if_block<'a>(keyword: Keyword) -> impl FnMut(Span<'a>) -> ParseResult<CondBlock> {
        map(
            pair(
                // Read the keyword, the condition, and the `then`.
                delimited(
                    pair(token(keyword), multi_trivia),
                    term,
                    pair(
                        context("Expected `then`!", token(Keyword::Then)),
                        semicolon_check(Keyword::Then),
                    ),
                ),
                // Parse the body, bailing if it's empty.
                alt((
                    term,
                    cut(preceded(
                        peek(alt((
                            token(Keyword::Fi),
                            token(Keyword::Elif),
                            token(Keyword::Else),
                        ))),
                        context("Empty `then` clause!", fail),
                    )),
                )),
            ),
            |(cond, block)| CondBlock::new(cond, block),
        )
    }

    fn semicolon_check(keyword: Keyword) -> impl Fn(Span) -> ParseResult<()> {
        move |span| {
            // Warn about `then;`s.
            let (span, semi) = terminated(opt(ranged(token(ControlOp::Semi))), multi_trivia)(span)?;
            if let Some((_, range)) = semi {
                span.extra.report(Lint::with_message(
                    range,
                    DISALLOWED_SEMI,
                    format_disallowed_semi(keyword.token()),
                ));
            }

            Ok((span, ()))
        }
    }

    map(
        tuple((
            if_block(Keyword::If),
            many0(if_block(Keyword::Elif)),
            terminated(
                opt(else_branch),
                context("Expected `fi`!", token(Keyword::Fi)),
            ),
        )),
        |(if_branch, elif_branches, else_term)| IfCmd::new(if_branch, elif_branches, else_term),
    )(span)
}

fn in_list(span: Span) -> ParseResult<Vec<Word>> {
    // Parse the `in` keyword.
    let (span, _) = token(Keyword::In)(span)?;

    // Read the list and stop when we hit a `do`, new line or semicolon.
    let (span, (list, _)) = many_till(
        terminated(word, trivia),
        alt((
            |span| {
                let (span, _) = peek(token(Keyword::Do))(span)?;
                // `do` should be on a new line or statement:
                span.extra.report(Lint::new(Range::from(&span), INLINED_DO));

                Ok((span, ()))
            },
            terminated(
                alt((swallow(token(ControlOp::Semi)), swallow(line_ending))),
                multi_trivia,
            ),
        )),
    )(span)?;

    Ok((span, list))
}

fn in_listed(span: Span) -> ParseResult<InListed> {
    // Parse the iterator variable.
    let (span, ((dollar, name), range)) =
        terminated(ranged(pair(opt(char('$')), identifier)), multi_trivia)(span)?;

    // Emit lint if there's a dollar (the iterator variable should be an
    // identifier, not a value).
    if dollar.is_some() {
        span.extra.report(Lint::new(range, DOLLAR_IN_LIST_NAME));
    }

    // Parse the iterating list.
    let (span, list) = alt((
        in_list,
        map(
            opt(alt((
                preceded(token(ControlOp::Semi), linebreak),
                newline_list,
            ))),
            |_| vec![],
        ),
    ))(span)?;

    // Parse the body.
    let (span, body) = alt((brace_group, do_group))(span)?;

    Ok((span, InListed::new(name, list, body)))
}

fn linebreak(span: Span) -> ParseResult<()> {
    swallow(opt(newline_list))(span)
}

fn line_ending(span: Span) -> ParseResult<char> {
    // TODO: readPendingHereDocs
    preceded(opt(carriage_return), newline)(span)
}

fn line_space(span: Span) -> ParseResult<char> {
    alt((one_of(" \t"), |span| {
        let (span, (_, range)) = ranged(one_of(UNISPACES))(span)?;
        span.extra
            .report(Lint::with_message(range, UNICHAR, format_unichar("space")));

        Ok((span, ' '))
    }))(span)
}

fn lit_sgmt(end_pattern: &'static str) -> impl Fn(Span) -> ParseResult<Lit> {
    fn escaped_lit(span: Span) -> ParseResult<String> {
        let (mut span, (start_span, c)) = preceded(
            backslash,
            pair(
                nom_locate::position,
                alt((
                    line_space,
                    one_of(DOUBLE_ESCAPABLE),
                    one_of(&*format!("|&;<>()'\n\r[]{{}}.,~#{}", EXTGLOB_PREFIX)),
                )),
            ),
        )(span)?;

        if c == ' ' {
            // Backslash followed by whitespace breaks the line break.
            let trailing_space;
            (span, trailing_space) = opt(pair(
                many0(line_space),
                alt((line_ending, value(char::default(), eof))),
            ))(span)?;

            if trailing_space.is_some() {
                span.extra
                    .report(Lint::new(Range::new(start_span, &span), BS_TRAILING_SPACE));
            }
        }

        Ok((span, c.into()))
    }

    fn failed_escaped(span: Span) -> ParseResult<String> {
        // Looks like an escaped character, but isn't.
        let (span, (c, range)) = ranged(preceded(backslash, anychar))(span)?;

        if let Some((name, alt)) = match c {
            'n' => Some(("new line", "a quoted, literal new line")),
            't' => Some(("tab", "`$(printf 't')`")),
            'r' => Some(("carriage return", "`(printf 'r')`")),
            _ => None,
        } {
            span.extra.report(Lint::with_message(
                range,
                UNESCAPED_WHITESPACE,
                format_unescaped_whitespace(c, name, alt),
            ));
        } else {
            span.extra.report(Lint::with_message(
                range,
                IGNORING_BS,
                format_ignoring_bs(c),
            ));
        }

        Ok((span, c.into()))
    }

    move |span| {
        map(
            many1(alt((
                // Special case for line continuation + commented sequences.
                // By calling trivia() after the peek, we make sure that the lint for `# foo \` is reported.
                value(
                    String::new(),
                    terminated(peek(pair(backslash, newline)), trivia),
                ),
                alt((escaped_lit, failed_escaped)),
                // Some other sequence of non-special, unescaped characters.
                parsed(is_not(&*format!(
                    "[{{}}|&;<>() '\t\n\r\u{A0}{}{}{}{}{}",
                    EXTGLOB_PREFIX,
                    DOUBLE_ESCAPABLE,
                    SINGLE_UNIQUOTES,
                    DOUBLE_UNIQUOTES,
                    end_pattern
                ))),
            ))),
            |lits| Lit::new(lits.concat()),
        )(span)
    }
}

fn lit_word_sgmt<'a>(end_pattern: &'static str) -> impl FnMut(Span<'a>) -> ParseResult<WordSgmt> {
    into(lit_sgmt(end_pattern))
}

fn multi_trivia(span: Span) -> ParseResult<String> {
    map(separated_list0(line_ending, trivia), |trivia| {
        trivia.join("\n")
    })(span)
}

fn multi_trivia1(span: Span) -> ParseResult<String> {
    context(
        "Expected whitespace!",
        verify(multi_trivia, |trivia: &String| !trivia.is_empty()),
    )(span)
}

fn newline_list(span: Span) -> ParseResult<()> {
    // Parse all the line separators.
    let (span, _) = many1(terminated(alt((line_ending, carriage_return)), trivia))(span)?;

    // If we peek a control operator, the last line was probably broken incorrectly.
    let (span, op) = opt(peek(ranged(alt((
        token(ControlOp::AndIf),
        token(ControlOp::OrIf),
        token(ControlOp::Or),
    )))))(span)?;
    if let Some((op, range)) = op {
        span.extra.report(Lint::with_message(
            range,
            STARTING_CONTROL,
            format_starting_control(op.token()),
        ));
    }

    Ok((span, ()))
}

fn pipeline(span: Span) -> ParseResult<Pipeline> {
    // Ensure there's not a misplaced token.
    let (span, _) = context("Unexpected token", not(peek_sus_token))(span)?;

    // Parse a bang if there's one.
    let (span, _) = opt(|span| {
        let (span, trivia) = preceded(char('!'), trivia)(span)?;
        if trivia.is_empty() {
            span.extra
                .report(Lint::new(Range::from(&span), MISSING_SPACE));
        }

        Ok((span, ()))
    })(span)?;

    // Parse the commands.
    let (span, cmds) = separated_list1(
        context(
            "Invalid pipeline operator!",
            delimited(
                // Make sure we match `|` and `|&` but not `||`.
                not(token(ControlOp::OrIf)),
                alt((token(ControlOp::OrAnd), token(ControlOp::Or))),
                pair(trivia, linebreak),
            ),
        ),
        cmd,
    )(span)?;

    // Spacing and bye.
    let (span, _) = trivia(span)?;

    Ok((span, Pipeline::new(cmds)))
}

fn redir(span: Span) -> ParseResult<Redir> {
    // Read the optional file descriptor.
    let (span, desc) = context(
        "Invalid file descriptor!",
        alt((
            map(
                alt((
                    map(char('&'), |_| FileDesc::StdOutErr),
                    map(delimited(char('{'), identifier, char('}')), |ident| {
                        Variable::new(ident).into()
                    }),
                    map(digit1, |_| FileDesc::Number),
                )),
                Some,
            ),
            // // Make sure the operator follows if we didn't parse a descriptor.
            map(peek(one_of("<>")), |_| None),
        )),
    )(span)?;

    // Parse the redirection operator and word.
    let (span, (op, word)) = preceded(
        context("Expected a redirection operator!", peek(one_of("<>"))),
        alt((
            // Here strings:
            pair(token(RedirOp::TLess), word),
            // TODO: readHereDoc
            // File descriptor duplication:
            pair(
                alt((token(RedirOp::GreatAnd), token(RedirOp::LessAnd))),
                // The word can be a variable, or a digit optionally followed by a dash,
                // or a dash.
                map(
                    alt((
                        delimited(char('{'), identifier, char('}')),
                        parsed(pair(digit1, opt(char('-')))),
                        parsed(char('-')),
                    )),
                    |word| Word::new(vec![Lit::new(word).into()]),
                ),
            ),
            // Redirection from/to a file.
            pair(
                alt((
                    token(RedirOp::DGreat),
                    token(RedirOp::LessGreat),
                    token(RedirOp::GreatAnd),
                    token(RedirOp::LessAnd),
                    token(RedirOp::Clobber),
                    token(RedirOp::DLess),
                    token(RedirOp::Less),
                    token(RedirOp::Great),
                )),
                word,
            ),
        )),
    )(span)?;

    // Spacing and bye.
    let (span, _) = trivia(span)?;

    Ok((span, Redir::new(desc, op, word)))
}

fn simple_cmd(span: Span) -> ParseResult<SimpleCmd> {
    fn let_seq(span: Span) -> ParseResult<CmdSuffixSgmt> {
        // Read the input math word.
        let (span, (seq_start, seq_span)) =
            pair(nom_locate::position, terminated(recognize(word), trivia))(span)?;

        // Remove quotes.
        let (mut seq_span, quote) = opt(one_of("'\""))(seq_span)?;
        if let Some(quote) = quote {
            (_, seq_span) =
                all_consuming(terminated(is_not(&*String::from(quote)), char(quote)))(seq_span)?;
        }

        // Subparse the math expression.
        let seq_span = unsafe {
            Span::new_from_raw_offset(
                seq_start.location_offset(),
                seq_start.location_line(),
                &seq_span,
                Context::new(),
            )
        };
        let (seq_span, seq) = all_consuming(arith_seq)(seq_span)?;
        span.extra.extend_lints(seq_span.extra);

        Ok((span, seq.into()))
    }

    // Parse the prefix and the command.
    let (mut span, (prefix, cmd)) = verify(
        pair(
            many0(alt((into(redir), into(assign)))),
            opt(delimited(
                // Ignore alias supression.
                opt(pair(
                    backslash,
                    peek(satisfy(|c| {
                        c.is_ascii_alphanumeric() || matches!(c, '_' | '.' | ':')
                    })),
                )),
                ranged(word),
                trivia,
            )),
        ),
        |(prefix, cmd)| cmd.is_some() || !prefix.is_empty(),
    )(span)?;

    let mut suffix = vec![];
    if let Some((cmd, range)) = &cmd {
        // Check if the command looks like a comment.
        if let WordSgmt::Lit(lit) = &cmd.sgmts()[0] {
            if lit.value().starts_with("//") {
                span.extra.report(Lint::new(*range, C_LIKE_COMMENT));
            }

            if let Some(WordSgmt::Glob(glob)) = &cmd.sgmts().get(1) {
                if lit.value() == "/" && glob == "*" {
                    span.extra.report(Lint::new(*range, C_LIKE_COMMENT));
                }
            }
        }

        if let Some(cmd) = cmd.as_lit() {
            let cmd = cmd.value();

            // Not using the right keyword for an "else if" statement.
            if matches!(&*cmd.to_ascii_lowercase(), "elsif" | "elseif") {
                span.extra.report(Lint::new(*range, ELIF_LIKE));
            }

            // When using "builtin", the first argument is the actual shell command.
            let cmd = if cmd == "builtin" {
                let builtin;
                (span, builtin) = opt(map(terminated(word, trivia), |word| {
                    word.as_lit().map(|lit| String::from(lit.value()))
                }))(span)?;
                builtin.flatten().unwrap_or_default()
            } else {
                cmd.into()
            };

            // Get the suffix based on the command.
            (span, suffix) = match &*cmd {
                "declare" | "export" | "local" | "readonly" | "typeset" => {
                    many1(alt((
                        into(redir),
                        into(assign),
                        into(terminated(word, trivia)),
                    )))(span)?
                }
                "eval" => many1(alt((
                    into(redir),
                    into(terminated(word, trivia)),
                    cut(context(
                        "Quote or escape your parens when using `eval`!",
                        preceded(char('('), fail),
                    )),
                )))(span)?,
                "let" => context(
                    "Expected an expression for this `let` command!",
                    many1(alt((into(redir), let_seq, into(terminated(word, trivia))))),
                )(span)?,
                "time" => preceded(
                    // Read but ignore flag arguments.
                    many0(delimited(peek(char('-')), word, trivia)),
                    // Parse the input pipeline, if any.
                    map(opt(pipeline), |pipeline| {
                        pipeline.map_or(vec![], |pipeline| vec![pipeline.into()])
                    }),
                )(span)?,
                _ => many0(alt((into(redir), into(terminated(word, trivia)))))(span)?,
            }
        }
    }

    // TODO: readSource
    // TODO: syntaxCheckTrap

    // Drop the range.
    let cmd = cmd.map(|(cmd, _)| cmd);

    // Ignore flag arguments.
    suffix.retain(|sgmt| {
        if let CmdSuffixSgmt::Word(word) = sgmt {
            word.as_lit()
                .map_or(true, |lit| !lit.value().starts_with('-'))
        } else {
            true
        }
    });

    Ok((span, SimpleCmd::new(cmd, prefix, suffix)))
}

fn single_quoted(span: Span) -> ParseResult<SingleQuoted> {
    // Parse the opening quote and the string.
    let (span, (start_quote, string)) = pair(
        tag("'"),
        map(
            many0(alt((
                into(single_uniquote),
                parsed(is_not(&*format!("'{}", SINGLE_UNIQUOTES))),
            ))),
            |sgmt| sgmt.concat(),
        ),
    )(span)?;

    // Check that the ending quote isn't escaped.
    let last_char = string.chars().last().unwrap_or_default();
    if last_char == '\\' {
        span.extra
            .report(Lint::new(Range::from(&span), UNESCAPED_SINGLE_QUOTE));
    }

    // Verify the closing quote.
    let (span, (end_quote, range)) = ranged(cut(context(
        "Expected end of single quoted string!",
        tag("'"),
    )))(span)?;

    // Is the string unclosed?
    let (span, next_char) = opt(peek(satisfy(|c| is_sus_char_after_quote(c) || c == '\'')))(span)?;
    if let Some(next_char) = next_char {
        if next_char.is_ascii_alphabetic() && last_char.is_ascii_alphabetic() {
            span.extra.report(Lint::new(range, APOSTROPHE));
        } else if !string.starts_with('\n') && string.contains('\n') {
            span.extra.report_unclosed_string(&start_quote, &end_quote);
        }
    }

    Ok((span, SingleQuoted::new(string)))
}

fn single_uniquote(span: Span) -> ParseResult<char> {
    let (span, (quote, range)) = ranged(one_of(SINGLE_UNIQUOTES))(span)?;
    span.extra.report(Lint::with_message(
        range,
        UNICHAR,
        format_unichar("single quote"),
    ));

    Ok((span, quote))
}

fn subshell(span: Span) -> ParseResult<Term> {
    delimited(
        // Opening parenthesis and whitespace:
        pair(char('('), multi_trivia),
        // The subshell's list:
        term,
        // Closing parenthesis and whitespace:
        tuple((
            multi_trivia,
            context("Expected a closing `)`!", char(')')),
            trivia,
        )),
    )(span)
}

fn param_expansion(span: Span) -> ParseResult<ParamExpansion> {
    map(
        delimited(
            tag("${"),
            many0(alt((
                into(single_quoted),
                into(double_quoted),
                // Special characters in parameter expansions.
                into(lit_parser(parsed(is_a("/:+-=%")))),
                into(extglob),
                unquoted_dollar_sgmt,
                into(backquoted(false)),
                map(
                    // Literals, with maybe some escaped characters.
                    many1(alt((
                        escaped(BRACED_ESCAPABLE),
                        parsed(is_not(BRACED_ESCAPABLE)),
                    ))),
                    |lits| Lit::new(lits.concat()).into(),
                ),
            ))),
            char('}'),
        ),
        ParamExpansion::new,
    )(span)
}

fn peek_sus_token(span: Span) -> ParseResult<()> {
    // Tokens that are often misplaced in compound commands.
    alt((
        swallow(peek(alt((
            token(Keyword::Do),
            token(Keyword::Done),
            token(Keyword::Else),
            token(Keyword::Esac),
            token(Keyword::Elif),
            token(Keyword::Fi),
            token(Keyword::Then),
        )))),
        swallow(peek(one_of("})"))),
        swallow(peek(token(ControlOp::DSemi))),
    ))(span)
}

fn proc_sub(span: Span) -> ParseResult<ProcSub> {
    map(
        delimited(
            tuple((one_of("<>"), char('('), multi_trivia)),
            many0(term),
            pair(multi_trivia, char(')')),
        ),
        ProcSub::new,
    )(span)
}

fn term(span: Span) -> ParseResult<Term> {
    fn and_or(span: Span) -> ParseResult<Term> {
        // Left-fold the pipelines into a list.
        preceded(
            multi_trivia,
            map(
                pair(
                    map(pipeline, Term::Pipeline),
                    many0(separated_pair(
                        alt((token(ControlOp::AndIf), token(ControlOp::OrIf))),
                        linebreak,
                        pipeline,
                    )),
                ),
                |(head, tail)| {
                    tail.into_iter().fold(head, |acc, (op, pipeline)| {
                        List::new(acc, pipeline, op).into()
                    })
                },
            ),
        )(span)
    }

    fn and_sep(span: Span) -> ParseResult<ControlOp> {
        let (span, (sep_start, and)) = pair(nom_locate::position, token(ControlOp::And))(span)?;

        // Warn if the `&` seems to begin an HTML entity.
        let (mut span, entity_end) = opt(peek(preceded(
            pair(
                alt((
                    preceded(char('#'), digit1),
                    preceded(tag("#x"), alphanumeric1),
                    alpha1,
                )),
                char(';'),
            ),
            nom_locate::position,
        )))(span)?;
        if let Some(entity_end) = entity_end {
            span.extra
                .report(Lint::new(Range::new(&sep_start, entity_end), HTML_ENTITY));
        } else {
            // Warn if the `&` might be ending a command by mistake (e.g. it is in a URL).
            let alpha_char;
            (span, alpha_char) =
                followed_by(satisfy(|c| c == '_' || c.is_ascii_alphabetic()))(span)?;
            if alpha_char {
                span.extra
                    .report(Lint::new(Range::new(&sep_start, &span), UNSPACED_AMP));
            }
        }

        // Warn about redundant `;`s.
        let (span, semi_end) = preceded(
            trivia,
            opt(delimited(
                token(ControlOp::Semi),
                nom_locate::position,
                not(token(ControlOp::Semi)),
            )),
        )(span)?;
        if let Some(semi_end) = semi_end {
            span.extra
                .report(Lint::new(Range::new(sep_start, semi_end), AMP_SEMI));
        }

        Ok((span, and))
    }

    fn sep(span: Span) -> ParseResult<ControlOp> {
        alt((
            terminated(
                alt((
                    terminated(
                        and_sep,
                        // Don't parse `&&` or `&>`.
                        not(one_of("&>")),
                    ),
                    // Don't parse clause operators.
                    terminated(token(ControlOp::Semi), not(one_of(";&"))),
                )),
                linebreak,
            ),
            value(ControlOp::Newline, newline_list),
        ))(span)
    }

    let (span, term) = map(pair(and_or, many0(pair(sep, and_or))), |(head, tail)| {
        tail.into_iter()
            .fold(head, |acc, (op, and_or)| List::new(acc, and_or, op).into())
    })(span)?;

    // Read the optional terminator.
    let (span, _) = opt(sep)(span)?;

    Ok((span, term))
}

fn trivia(span: Span) -> ParseResult<String> {
    fn line_continuation(span: Span) -> ParseResult<Vec<char>> {
        let (span, (mut continued, comment)) = preceded(
            pair(backslash, newline),
            // The line was continued, check if this line is a comment with an escaped new line.
            pair(many0(line_space), opt(comment)),
        )(span)?;

        if let Some(comment) = comment {
            // Line continuations at the end of a comment are not actually line continuations.
            if comment.ends_with('\\') {
                span.extra
                    .report(Lint::new(Range::from(&span), COMMENTED_BS_LF));
            }

            continued.append(&mut comment.chars().collect());
        }

        Ok((span, continued))
    }

    map(
        pair(
            map(
                many0(alt((many1(line_space), line_continuation))),
                |trivia| trivia.into_iter().flatten().collect::<String>(),
            ),
            opt(comment),
        ),
        |(mut trivia, comment)| {
            // Include the trailing comment.
            trivia.extend(comment);

            trivia
        },
    )(span)
}

fn trivia1(span: Span) -> ParseResult<String> {
    context(
        "Expected whitespace!",
        verify(trivia, |trivia: &str| !trivia.is_empty()),
    )(span)
}

fn unquoted_dollar_sgmt(span: Span) -> ParseResult<WordSgmt> {
    alt((
        into(alt((
            dollar_exp,
            // $"" and $'' strings.
            preceded(char('$'), alt((into(double_quoted), into(single_quoted)))),
        ))),
        // A lonely dollar.
        into(lit_parser(char('$'))),
    ))(span)
}

fn whitespace(span: Span) -> ParseResult<char> {
    alt((line_space, carriage_return, line_ending))(span)
}

fn word(span: Span) -> ParseResult<Word> {
    let (span, (word, range)) = ranged(map(
        context("Expected a non-empty word!", many1(word_sgmt(""))),
        Word::new,
    ))(span)?;

    // Check for misplaced keywords.
    if let Some(lit) = word.as_lit() {
        let lit = lit.value();
        if vec![
            Keyword::Do,
            Keyword::Done,
            Keyword::Esac,
            Keyword::Fi,
            Keyword::Then,
        ]
        .into_iter()
        .any(|keyword| keyword.token() == lit)
        {
            span.extra.report(Lint::with_message(
                range,
                LITERAL_KEYWORD,
                format_literal_keyword(lit),
            ));
        }
    }

    Ok((span, word))
}

fn word_sgmt<'a>(end_pattern: &'static str) -> impl FnMut(Span<'a>) -> ParseResult<WordSgmt> {
    fn lit_curly(span: Span) -> ParseResult<Lit> {
        // Curly that's not a keyword.
        let (span, (curly, range)) = ranged(one_of("{}"))(span)?;
        span.extra
            .report(Lint::with_message(range, LITERAL_KEYWORD, "curly brace"));

        Ok((span, Lit::new(curly)))
    }

    move |span| {
        // Check that the segment isn't the end pattern.
        let (span, _) = not(one_of(end_pattern))(span)?;

        // Word segments can't begin with a parenthesis.
        let (span, _) = context(
            "`(` is invalid here. Did you forget to escape it?",
            not(char('(')),
        )(span)?;

        alt((
            into(single_quoted),
            into(double_quoted),
            into(extglob),
            // Regex match characters:
            map(one_of("*?"), |c| Glob::new(c).into()),
            into(bracketed_glob),
            // Fallback for other glob prefix characters:
            into(lit_parser(one_of("@!+["))),
            unquoted_dollar_sgmt,
            into(brace_expansion),
            into(backquoted(false)),
            into(proc_sub),
            into(lit_parser(alt((single_uniquote, double_uniquote)))),
            lit_word_sgmt(end_pattern),
            // Literal curly braces:
            into(alt((lit_parser(parsed(tag("{}"))), lit_curly))),
        ))(span)
    }
}
// endregion

pub(crate) fn parse(source: &str) {
    let res = function(source_to_span(source));
    println!("{:#?}", res);
}
