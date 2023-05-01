use super::*;

fn arith_seq(span: Span) -> ParseResult<ArithSeq> {
    macro_rules! bin_op {
        ($op:path) => {
            ::nom::sequence::terminated(
                $crate::parser::token::token($op),
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
                        span.extra.diag(
                            ParseDiagnostic::new(ParseDiagnosticKind::BadOperator).label(
                                format!(
                                    "for math stuff, use {} instead of -{}.",
                                    math_op.token(),
                                    test_op
                                ),
                                Range::new(start_span, end_span),
                            ),
                        );
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
        swallow(many0(alt((swallow(whitespace), line_continuation))))(span)
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
                            map(recognize_string(arith_seq), Subscript::new),
                            char(']'),
                        )),
                    ),
                    |(ident, indices)| Variable::with_subscripts(ident, indices).into(),
                ),
                map(
                    many1(alt((
                        into(single_quoted),
                        // TODO into(double_quoted),
                        // TODO unquoted_dollar_sgmt,
                        // TODO into(brace_expansion),
                        // TODO into(backquoted(false)),
                        into(lit(char('#'))),
                        // Parse a literal until something that looks like a math operator.
                        // TODO lit_word_sgmt("+-*/=%^,]?:"),
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::tests;

    #[test]
    fn test_arith_seq() {
        // A single digit is fine.
        assert_parse!(arith_seq("0") => "", ArithSeq::new(vec![tests::arith_number("0").into()]));
        assert_parse!(
            arith_seq("!!0") => "",
            ArithSeq::new(vec![
                ArithUnExpr::new(
                    ArithUnExpr::new(tests::arith_number("0"), UnOp::Not),
                    UnOp::Not,
                ).into()
            ])
        );

        // Pre and post increments and decrements.
        assert_parse!(
            arith_seq("x++ + --y") => "",
            ArithSeq::new(vec![
                ArithBinExpr::new(
                    ArithUnExpr::new(Variable::new("x"), UnOp::Inc),
                    ArithUnExpr::new(Variable::new("y"), UnOp::Dec),
                    BinOp::Add
                ).into()
            ])
        );

        // Trinary conditions with weird spacing.
        assert_parse!(
            arith_seq("a?\\\nb:c?\td : e") => "",
            ArithSeq::new(vec![
                ArithTriExpr::new(
                    Variable::new("a"),
                    Variable::new("b"),
                    ArithTriExpr::new(Variable::new("c"), Variable::new("d"), Variable::new("e")),
                ).into()
            ])
        );

        // Groups and dollar expressions.
        // TODO: Uncomment when adding dollar expressions.
        // assert_parse!(
        //     arith_seq("($x ** 2)") => "",
        //     ArithSeq::new(vec![
        //         ArithGroup::new(ArithSeq::new(vec![
        //             ArithBinExpr::new(
        //                 ArithExpansion::new(vec![
        //                     DollarExp::Variable(Variable::new("x")).into(),
        //                 ]),
        //                 tests::arith_number("2"),
        //                 BinOp::Pow,
        //             ).into(),
        //         ])).into()
        //     ])
        // );

        // A list of assignments.
        assert_parse!(
            arith_seq("x+=1, y<<=2") => "",
            ArithSeq::new(vec![
                ArithBinExpr::new(Variable::new("x"), tests::arith_number("1"), BinOp::AddEq).into(),
                ArithBinExpr::new(Variable::new("y"), tests::arith_number("2"), BinOp::ShlEq).into()
            ])
        );

        // Lint for using a test operator inside math stuff.
        assert_parse!(
            arith_seq("x -lt") => "",
            ArithSeq::new(vec![
                ArithBinExpr::new(Variable::new("x"), Variable::new("lt"), BinOp::Sub).into()
            ]),
            [((1, 3), (1, 6), ParseDiagnosticKind::BadOperator)]
        );
    }
}
