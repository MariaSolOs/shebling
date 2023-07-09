use super::*;

pub(super) const EXTGLOB_PREFIX: &str = "?*@!+";
const BRACED_ESCAPABLE: &str = "}\"$`'";
/// Note that `$0` is not included here, we handle numerical variables separately.
const SPECIAL_PARAMS: &str = "$?!#-@*";

fn arith_seq(span: ParseSpan) -> ParseResult<ArithSeq> {
    macro_rules! bin_op {
        ($op:path) => {
            ::nom::sequence::terminated(
                $crate::parser::token::token($op),
                ::nom::combinator::not(::nom::character::complete::one_of("&|<>="))
            )
        };

        (($($op:path),+)) => { ::nom::branch::alt(($(bin_op!($op),)+)) };
    }

    fn addition(span: ParseSpan) -> ParseResult<ArithTerm> {
        chained(
            multiplication,
            alt((
                bin_op!(BinOp::Add),
                // Read binary minus, but check if it's a binary test operator.
                |span| {
                    let (span, (start, test_op)) = separated_pair(
                        offset,
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
                            offset,
                        ))),
                    )(span)?;

                    if let Some(((test_op, math_op), end)) = test_op {
                        span.diag(Diagnostic::builder(DiagnosticKind::BadOperator).label(
                            format!(
                                "for math stuff, use {} instead of -{}",
                                math_op.token(),
                                test_op
                            ),
                            Span::new(start, end),
                        ));
                    }

                    Ok((span, BinOp::Sub))
                },
            )),
        )(span)
    }

    fn and(span: ParseSpan) -> ParseResult<ArithTerm> {
        chained(bit_or, bin_op!(BinOp::And))(span)
    }

    fn arith_space(span: ParseSpan) -> ParseResult<()> {
        swallow(many0(alt((swallow(whitespace), line_continuation))))(span)
    }

    fn assignment(span: ParseSpan) -> ParseResult<ArithTerm> {
        map(
            pair(
                trinary,
                many0(separated_pair(
                    spanned(bin_op!((
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
                    ))),
                    arith_space,
                    trinary,
                )),
            ),
            |(head, mut tail)| {
                // This is basically a right fold (right-associative application of
                // the assignments).
                tail.reverse();
                if let Some((op, term)) = tail.into_iter().reduce(|(op, right), (next_op, left)| {
                    (next_op, BinExpr::new(left, right, op).into())
                }) {
                    BinExpr::new(head, term, op).into()
                } else {
                    head
                }
            },
        )(span)
    }

    fn bit_and(span: ParseSpan) -> ParseResult<ArithTerm> {
        chained(equated, bin_op!(BinOp::BitAnd))(span)
    }

    fn bit_or(span: ParseSpan) -> ParseResult<ArithTerm> {
        chained(bit_xor, bin_op!(BinOp::BitOr))(span)
    }

    fn bit_xor(span: ParseSpan) -> ParseResult<ArithTerm> {
        chained(bit_and, bin_op!(BinOp::BitXor))(span)
    }

    fn chained<'a, T, O>(term: T, op: O) -> impl FnMut(ParseSpan<'a>) -> ParseResult<ArithTerm>
    where
        T: Fn(ParseSpan) -> ParseResult<ArithTerm> + Copy,
        O: FnMut(ParseSpan<'a>) -> ParseResult<BinOp>,
    {
        // Parse >= 1 terms and then apply the operator in a left associative way.
        map(
            pair(term, many0(separated_pair(spanned(op), arith_space, term))),
            |(head, tail)| {
                tail.into_iter()
                    .fold(head, |acc, (op, term)| BinExpr::new(acc, term, op).into())
            },
        )
    }

    fn compared(span: ParseSpan) -> ParseResult<ArithTerm> {
        chained(shift, bin_op!((BinOp::Le, BinOp::Ge, BinOp::Lt, BinOp::Gt)))(span)
    }

    fn equated(span: ParseSpan) -> ParseResult<ArithTerm> {
        chained(compared, bin_op!((BinOp::EqEq, BinOp::Ne)))(span)
    }

    fn exponential(span: ParseSpan) -> ParseResult<ArithTerm> {
        chained(maybe_negated, bin_op!(BinOp::Pow))(span)
    }

    fn maybe_incremented_term(span: ParseSpan) -> ParseResult<ArithTerm> {
        alt((
            // Postfix incremented (e.g. x++).
            map(
                pair(
                    terminated(term, arith_space),
                    terminated(
                        spanned(alt((token(UnOp::Inc), token(UnOp::Dec)))),
                        arith_space,
                    ),
                ),
                |(expr, op)| UnExpr::new(expr, op).into(),
            ),
            // Doesn't have a prefix or postfix increment.
            terminated(term, arith_space),
            // Prefix incremented (e.g. ++x).
            map(
                separated_pair(
                    spanned(alt((token(UnOp::Inc), token(UnOp::Dec)))),
                    arith_space,
                    term,
                ),
                |(op, expr)| UnExpr::new(expr, op).into(),
            ),
        ))(span)
    }

    fn maybe_negated(span: ParseSpan) -> ParseResult<ArithTerm> {
        alt((
            negated,
            // Signed term (e.g. +x, -x).
            map(
                separated_pair(
                    spanned(alt((
                        // Make sure we don't match the inc/dec terms.
                        terminated(token(UnOp::Pos), not(token(UnOp::Pos))),
                        terminated(token(UnOp::Neg), not(token(UnOp::Neg))),
                    ))),
                    arith_space,
                    maybe_incremented_term,
                ),
                |(op, expr)| UnExpr::new(expr, op).into(),
            ),
            maybe_incremented_term,
        ))(span)
    }

    fn multiplication(span: ParseSpan) -> ParseResult<ArithTerm> {
        chained(exponential, bin_op!((BinOp::Mult, BinOp::Div, BinOp::Mod)))(span)
    }

    fn negated(span: ParseSpan) -> ParseResult<ArithTerm> {
        map(
            separated_pair(
                spanned(alt((token(UnOp::Not), token(UnOp::BitNeg)))),
                arith_space,
                maybe_negated,
            ),
            |(op, expr)| UnExpr::new(expr, op).into(),
        )(span)
    }

    fn or(span: ParseSpan) -> ParseResult<ArithTerm> {
        chained(and, bin_op!(BinOp::Or))(span)
    }

    fn seq(span: ParseSpan) -> ParseResult<ArithSeq> {
        preceded(
            arith_space,
            separated_list0(pair(char(','), arith_space), assignment),
        )(span)
    }

    fn shift(span: ParseSpan) -> ParseResult<ArithTerm> {
        chained(addition, bin_op!((BinOp::Shl, BinOp::Shr)))(span)
    }

    fn term(span: ParseSpan) -> ParseResult<ArithTerm> {
        terminated(
            alt((
                // A group.
                delimited(char('('), into(seq), char(')')),
                // A variable, which could be an array with more math stuff as an index.
                map(
                    pair(
                        spanned(identifier),
                        many0(delimited(
                            char('['),
                            // TODO: Parse these later based on the array being associative or not.
                            spanned(recognize_string(arith_seq)),
                            char(']'),
                        )),
                    ),
                    |(ident, indices)| SubscriptedVar::new(ident, indices).into(),
                ),
                into(many1(alt((
                    map(spanned(single_quoted), WordSgmt::SingleQuoted),
                    into(double_quoted),
                    dollar_sgmt,
                    map(brace_expansion, WordSgmt::BraceExpansion),
                    // TODO: map(backquoted, WordSgmt::BackQuoted),
                    map(spanned(recognize_string(char('#'))), WordSgmt::Lit),
                    // TODO: // Parse a literal until something that looks like a math operator.
                    lit_word_sgmt("+-*/=%^,]?:"),
                )))),
            )),
            arith_space,
        )(span)
    }

    fn trinary(span: ParseSpan) -> ParseResult<ArithTerm> {
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

pub(super) fn brace_expansion(span: ParseSpan) -> ParseResult<Vec<Word>> {
    fn braced(span: ParseSpan) -> ParseResult<Vec<Word>> {
        delimited(
            char('{'),
            context(
                "invalid sequence expression!",
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
                                return lit.inner().split_once("..").map_or(
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
            char('}'),
        )(span)
    }

    fn braced_word(span: ParseSpan) -> ParseResult<Word> {
        map(
            many0(alt((
                map(braced, WordSgmt::BraceExpansion),
                into(dollar_exp),
                map(spanned(single_quoted), WordSgmt::SingleQuoted),
                into(double_quoted),
                map(
                    spanned(map(
                        many1(alt((
                            escaped(""),
                            recognize_string(is_not(&*format!(
                                "{{}}\"$', \t\r\n{}",
                                trivia::UNISPACES
                            ))),
                        ))),
                        |lits| lits.concat(),
                    )),
                    WordSgmt::Lit,
                ),
            ))),
            Word::new,
        )(span)
    }

    braced(span)
}

pub(super) fn dollar_exp(span: ParseSpan) -> ParseResult<DollarExp> {
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
                        "expected a double )) to end the $((..)).",
                        char(')'),
                    )),
                ),
            ),
            // TODO: into(dollar_cmd_sub),
            // TODO: Consider error SC1102.
            // Math in $[].
            delimited(pair(char('$'), char('[')), into(arith_seq), char(']')),
            // TODO: into(dollar_cmd_expansion),
            into(param_expansion),
            // A variable.
            map(
                preceded(
                    char('$'),
                    spanned(alt((
                        recognize_string(satisfy(|c| c.is_ascii_digit())),
                        recognize_string(one_of(SPECIAL_PARAMS)),
                        identifier,
                    ))),
                ),
                DollarExp::Var,
            ),
        )),
    )(span)
}

pub(super) fn dollar_sgmt(span: ParseSpan) -> ParseResult<WordSgmt> {
    alt((
        into(alt((
            dollar_exp,
            // $"" and $'' strings.
            preceded(
                char('$'),
                alt((
                    into(double_quoted),
                    map(spanned(single_quoted), DollarExp::SingleQuoting),
                )),
            ),
        ))),
        // A lonely dollar.
        map(spanned(recognize_string(char('$'))), WordSgmt::Lit),
    ))(span)
}

pub(super) fn extglob(span: ParseSpan) -> ParseResult<String> {
    fn group(span: ParseSpan) -> ParseResult<String> {
        delimited(char('('), sgmt, char(')'))(span)
    }

    fn sgmt(span: ParseSpan) -> ParseResult<String> {
        recognize_string(separated_list0(
            char('|'),
            recognize_string(many0(alt((
                group,
                recognize_string(word_sgmt),
                recognize_string(many1(whitespace)),
                recognize_string(is_a("<>#;&")),
            )))),
        ))(span)
    }

    recognize_string(pair(one_of(EXTGLOB_PREFIX), group))(span)
}

fn param_expansion(span: ParseSpan) -> ParseResult<Vec<WordSgmt>> {
    delimited(
        tag("${"),
        many0(alt((
            map(spanned(single_quoted), WordSgmt::SingleQuoted),
            into(double_quoted),
            map(spanned(extglob), WordSgmt::Glob),
            dollar_sgmt,
            // TODO: map(backquoted, WordSgmt::BackQuoted),
            map(
                spanned(map(
                    // Literals, with maybe some escaped characters.
                    many1(alt((
                        escaped(BRACED_ESCAPABLE),
                        recognize_string(is_not(&*format!("\\{}", BRACED_ESCAPABLE))),
                    ))),
                    |lits| lits.concat(),
                )),
                WordSgmt::Lit,
            ),
        ))),
        char('}'),
    )(span)
}
