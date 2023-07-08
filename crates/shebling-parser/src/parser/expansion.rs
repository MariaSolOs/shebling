use super::*;

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
                // TODO: Variables.
                into(many1(alt((
                    map(spanned(single_quoted), WordSgmt::SingleQuoted),
                    into(double_quoted),
                    // TODO: Parse other segments.
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
