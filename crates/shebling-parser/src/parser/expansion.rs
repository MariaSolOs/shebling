use super::*;

const BRACED_ESCAPABLE: &str = "}\"$`'";
pub(super) const EXTGLOB_PREFIX: &str = "?*@!+";
/// [Special shell parameters](https://www.gnu.org/software/bash/manual/bash.html#Special-Parameters).
/// Note that `$0` is not included here, we handle numerical variables separately.
const SPECIAL_PARAMS: &str = "$?!#-@*";

pub(super) fn arith_seq(span: Span) -> ParseResult<ArithSeq> {
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
                    let (span, (start, test_op)) = separated_pair(
                        position,
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
                            position,
                        ))),
                    )(span)?;

                    if let Some(((test_op, math_op), end)) = test_op {
                        span.extra.diag(
                            ParseDiagnostic::builder(ParseDiagnosticKind::BadOperator).label(
                                format!(
                                    "for math stuff, use {} instead of -{}.",
                                    math_op.token(),
                                    test_op
                                ),
                                Range::new(start, end),
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
                    (next_op, BinExpr::new(left, right, op).into())
                }) {
                    BinExpr::new(head, term, op).into()
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
                tail.into_iter()
                    .fold(head, |acc, (op, term)| BinExpr::new(acc, term, op).into())
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
                |(expr, op)| UnExpr::new(expr, op).into(),
            ),
            // Doesn't have a prefix or postfix increment.
            terminated(term, arith_space),
            // Prefix incremented (e.g. ++x).
            map(
                separated_pair(alt((token(UnOp::Inc), token(UnOp::Dec))), arith_space, term),
                |(op, expr)| UnExpr::new(expr, op).into(),
            ),
        ))(span)
    }

    fn maybe_negated(span: Span) -> ParseResult<ArithTerm> {
        alt((
            negated,
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
                |(op, expr)| UnExpr::new(expr, op).into(),
            ),
            maybe_incremented_term,
        ))(span)
    }

    fn multiplication(span: Span) -> ParseResult<ArithTerm> {
        chained(exponential, bin_op!((BinOp::Mult, BinOp::Div, BinOp::Mod)))(span)
    }

    fn negated(span: Span) -> ParseResult<ArithTerm> {
        map(
            separated_pair(
                alt((token(UnOp::Not), token(UnOp::BitNeg))),
                arith_space,
                maybe_negated,
            ),
            |(op, expr)| UnExpr::new(expr, op).into(),
        )(span)
    }

    fn or(span: Span) -> ParseResult<ArithTerm> {
        chained(and, bin_op!(BinOp::Or))(span)
    }

    fn seq(span: Span) -> ParseResult<ArithSeq> {
        preceded(
            arith_space,
            separated_list0(pair(char(','), arith_space), assignment),
        )(span)
    }

    fn shift(span: Span) -> ParseResult<ArithTerm> {
        chained(addition, bin_op!((BinOp::Shl, BinOp::Shr)))(span)
    }

    fn term(span: Span) -> ParseResult<ArithTerm> {
        terminated(
            alt((
                // A group.
                delimited(char('('), into(seq), char(')')),
                // A variable, which could be an array with more math stuff as an index.
                map(
                    pair(
                        identifier,
                        many0(delimited(
                            char('['),
                            // TODO: Parse these later based on the array being associative or not.
                            recognize_string(arith_seq),
                            char(']'),
                        )),
                    ),
                    |(ident, indices)| SubscriptedVar::new(ident, indices).into(),
                ),
                into(many1(alt((
                    map(single_quoted, WordSgmt::SingleQuoted),
                    into(double_quoted),
                    dollar_sgmt,
                    map(brace_expansion, WordSgmt::BraceExpansion),
                    map(backquoted, WordSgmt::BackQuoted),
                    map(char('#'), |c| WordSgmt::Lit(c.into())),
                    // Parse a literal until something that looks like a math operator.
                    lit_word_sgmt("+-*/=%^,]?:"),
                )))),
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

pub(super) fn brace_expansion(span: Span) -> ParseResult<Vec<Word>> {
    fn braced(span: Span) -> ParseResult<Vec<Word>> {
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
                                return lit.split_once("..").map_or(false, |(prefix, suffix)| {
                                    !prefix.is_empty()
                                        && !prefix.contains("..")
                                        && !suffix.is_empty()
                                        && !suffix.contains("..")
                                });
                            }
                        }

                        true
                    },
                ),
            ),
            char('}'),
        )(span)
    }

    fn braced_word(span: Span) -> ParseResult<Word> {
        map(
            many0(alt((
                map(braced, WordSgmt::BraceExpansion),
                into(dollar_exp),
                map(single_quoted, WordSgmt::SingleQuoted),
                into(double_quoted),
                map(
                    many1(alt((
                        escaped(""),
                        recognize_string(is_not(&*format!("{{}}\"$', \t\r\n{}", UNISPACES))),
                    ))),
                    |lits| WordSgmt::Lit(lits.concat()),
                ),
            ))),
            Word::new,
        )(span)
    }

    braced(span)
}

fn dollar_cmd_expansion(span: Span) -> ParseResult<Term> {
    delimited(
        tuple((tag("${"), whitespace, multi_trivia)),
        term,
        context("expected a closing }", char('}')),
    )(span)
}

fn dollar_cmd_sub(span: Span) -> ParseResult<Option<Term>> {
    delimited(
        pair(tag("$("), multi_trivia),
        alt((
            // Parse the substitution, which can be empty.
            map(peek(alt((char(')'), value(char::default(), eof)))), |_| {
                None
            }),
            map(term, Some),
        )),
        context("expected a closing )", char(')')),
    )(span)
}

pub(super) fn dollar_exp(span: Span) -> ParseResult<DollarExp> {
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
            into(dollar_cmd_sub),
            // TODO: Consider error SC1102.
            // Math in $[].
            delimited(pair(char('$'), char('[')), into(arith_seq), char(']')),
            into(dollar_cmd_expansion),
            into(param_expansion),
            map(dollar_var, DollarExp::Var),
        )),
    )(span)
}

pub(super) fn dollar_sgmt(span: Span) -> ParseResult<WordSgmt> {
    alt((
        into(alt((
            dollar_exp,
            // $"" and $'' strings.
            preceded(
                char('$'),
                alt((
                    into(double_quoted),
                    map(single_quoted, DollarExp::SingleQuoting),
                )),
            ),
        ))),
        // A lonely dollar.
        map(char('$'), |c| WordSgmt::Lit(c.into())),
    ))(span)
}

fn dollar_var(span: Span) -> ParseResult<String> {
    fn dollar_ident(span: Span) -> ParseResult<String> {
        let (span, ((ident, range), has_bracket)) =
            pair(ranged(identifier), followed_by(char('[')))(span)?;
        if has_bracket {
            span.extra.diag(
                ParseDiagnostic::builder(ParseDiagnosticKind::Unbraced)
                .label("use braces when expanding arrays", range)
                .help(format!("Use ${{{}[..]}} to tell the shell that the square brackets are part of the expansion.", ident))
            );
        }

        Ok((span, ident))
    }

    fn dollar_digit(span: Span) -> ParseResult<String> {
        // First match a single digit:
        let (span, (start, dig)) = pair(position, satisfy(|c| c.is_ascii_digit()))(span)?;

        // Then read extra digits and warn because they need to be braced.
        let (span, extra) = opt(peek(pair(digit1, position)))(span)?;

        if let Some((extra_digs, end)) = extra {
            span.extra.diag(
                ParseDiagnostic::builder(ParseDiagnosticKind::Unbraced)
                .label("braces are required for multi-digit positionals", Range::new(start, end))
                .help(format!("${0}{1} is interpreted as ${0} followed by a literal {1}. Use ${{{0}{1}}} instead.", dig, extra_digs)),
            );
        }

        Ok((span, dig.into()))
    }

    preceded(
        char('$'),
        alt((
            dollar_digit,
            recognize_string(one_of(SPECIAL_PARAMS)),
            dollar_ident,
        )),
    )(span)
}

pub(super) fn extglob(span: Span) -> ParseResult<String> {
    fn group(span: Span) -> ParseResult<String> {
        delimited(char('('), sgmt, char(')'))(span)
    }

    fn sgmt(span: Span) -> ParseResult<String> {
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

fn param_expansion(span: Span) -> ParseResult<Vec<WordSgmt>> {
    delimited(
        tag("${"),
        many0(alt((
            map(single_quoted, WordSgmt::SingleQuoted),
            into(double_quoted),
            map(extglob, WordSgmt::Glob),
            dollar_sgmt,
            map(backquoted, WordSgmt::BackQuoted),
            map(
                // Literals, with maybe some escaped characters.
                many1(alt((
                    escaped(BRACED_ESCAPABLE),
                    recognize_string(is_not(&*format!("\\{}", BRACED_ESCAPABLE))),
                ))),
                |lits| WordSgmt::Lit(lits.concat()),
            ),
        ))),
        char('}'),
    )(span)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::tests;

    #[test]
    fn test_arith_seq() {
        // A single digit is fine.
        assert_parse!(arith_seq("0"), vec![tests::arith_number("0").into()]);
        assert_parse!(
            arith_seq("!!0"),
            vec![UnExpr::new(UnExpr::new(tests::arith_number("0"), UnOp::Not), UnOp::Not).into()]
        );

        // Pre and post increments and decrements.
        assert_parse!(
            arith_seq("x++ + --y"),
            vec![BinExpr::new(
                UnExpr::new(tests::var("x"), UnOp::Inc),
                UnExpr::new(tests::var("y"), UnOp::Dec),
                BinOp::Add
            )
            .into()]
        );

        // Trinary conditions with weird spacing.
        assert_parse!(
            arith_seq("a?\\\nb:c?\td : e"),
            vec![ArithTriExpr::new(
                tests::var("a"),
                tests::var("b"),
                ArithTriExpr::new(tests::var("c"), tests::var("d"), tests::var("e")),
            )
            .into()]
        );

        // Groups and dollar expressions.
        assert_parse!(
            arith_seq("($x ** 2)"),
            vec![ArithTerm::Group(vec![BinExpr::new(
                ArithTerm::Expansion(vec![DollarExp::Var("x".into()).into()]),
                tests::arith_number("2"),
                BinOp::Pow,
            )
            .into()])]
        );

        // A list of assignments.
        assert_parse!(
            arith_seq("x+=1, y<<=2"),
            vec![
                BinExpr::new(tests::var("x"), tests::arith_number("1"), BinOp::AddEq).into(),
                BinExpr::new(tests::var("y"), tests::arith_number("2"), BinOp::ShlEq).into()
            ]
        );

        // Lint for using a test operator inside math stuff.
        assert_parse!(
            arith_seq("x -lt"),
            vec![BinExpr::new(tests::var("x"), tests::var("lt"), BinOp::Sub).into()],
            [((1, 3), (1, 6), ParseDiagnosticKind::BadOperator)]
        );
    }

    #[test]
    fn test_brace_expansion() {
        // Valid sequence expressions.
        assert_parse!(brace_expansion("{1..5}"), vec![tests::word("1..5")]);
        assert_parse!(
            brace_expansion("{$x..$y}"),
            vec![Word::new(vec![
                DollarExp::Var("x".into()).into(),
                WordSgmt::Lit("..".into()),
                DollarExp::Var("y".into()).into(),
            ])]
        );

        // The closing brace can be escaped.
        assert_parse!(
            brace_expansion("{foo,\\}}"),
            vec![tests::word("foo"), tests::word("\\}")]
        );

        // Nested expansions are legal.
        assert_parse!(
            brace_expansion("{foo.{txt,md}}"),
            vec![Word::new(vec![
                WordSgmt::Lit("foo.".into()),
                WordSgmt::BraceExpansion(vec![tests::word("txt"), tests::word("md")]).into()
            ])]
        );

        // Part of the expansion can be an empty word.
        assert_parse!(
            brace_expansion("{,foo}"),
            vec![Word::new(vec![]), tests::word("foo")]
        );

        // Cannot be empty.
        assert_parse!(brace_expansion("{}") => Err((1, 2), Notes: [((1, 2), "invalid sequence")]));

        // If there's a single element, it must be a valid sequence expression.
        assert_parse!(brace_expansion("{foo}") => Err((1, 2), Notes: [((1, 2), "invalid sequence")]));
        assert_parse!(brace_expansion("{..1}") => Err((1, 2), Notes: [((1, 2), "invalid sequence")]));
    }

    #[test]
    fn test_dollar_cmd_expansion() {
        // The content can be any valid term.
        assert_parse!(
            dollar_cmd_expansion("${ foo; }"),
            tests::pipeline("foo").into()
        );

        // The term needs to end with a semicolon, else the closing curly will be
        // parsed as a literal.
        assert_parse!(dollar_cmd_expansion("${ foo }") => Err(
            (1, 9),
            Notes: [((1, 9), "expected a closing }")],
            Diags: [((1, 8), (1, 9), ParseDiagnosticKind::SusToken)]
        ));

        // There needs to be space after the {.
        assert_parse!(dollar_cmd_expansion("${foo; }") => Err(1, 3));
    }

    #[test]
    fn test_dollar_cmd_sub() {
        // The content can be any valid term.
        assert_parse!(
            dollar_cmd_sub("$( foo )"),
            Some(tests::pipeline("foo").into())
        );
        assert_parse!(
            dollar_cmd_sub("$(foo; ls 'bar')"),
            Some(
                List::new(
                    tests::pipeline("foo"),
                    Pipeline::new(vec![SimpleCmd::new(
                        Some(tests::word("ls")),
                        vec![],
                        vec![Word::new(vec![WordSgmt::SingleQuoted("bar".into())]).into()],
                    )
                    .into()]),
                    ControlOp::Semi
                )
                .into()
            )
        );

        // The content can be just trivia.
        assert_parse!(dollar_cmd_sub("$( )"), None);
        assert_parse!(dollar_cmd_sub("$(\n#foo\n)"), None);

        // Missing parentheses.
        assert_parse!(dollar_cmd_sub("$)") => Err(1, 1));
        assert_parse!(dollar_cmd_sub("$(") => Err((1, 3), Notes: [((1, 3), "expected a closing )")]));
    }

    #[test]
    fn test_dollar_exp() {
        // TODO: Add these after considering error SC1102.
    }

    #[test]
    fn test_dollar_var() {
        // A dollar followed by an identifier is valid.
        assert_parse!(dollar_var("$foo"), "foo");

        // If not an identifier, it must be a special parameter.
        assert_parse!(dollar_var("$?"), "?");

        // Numerical variables are fine:
        assert_parse!(dollar_var("$1"), "1");
        // ...but if they're multi-digit we emit a diagnostic.
        assert_parse!(
            dollar_var("$123") => "23",
            "1",
            [((1, 2), (1, 5), ParseDiagnosticKind::Unbraced)]
        );

        // Must begin with a dollar.
        assert_parse!(dollar_var("foo") => Err(1, 1));
    }

    #[test]
    fn test_extglob() {
        // The group can be empty.
        assert_parse!(extglob("*()"), "*()");

        // Nested groups and whitespace is allowed.
        assert_parse!(extglob("*(() | !(foo))"), "*(() | !(foo))");
    }

    #[test]
    fn test_param_expansion() {
        // The closing brace can be escaped.
        assert_parse!(
            param_expansion("${foo\\}}"),
            vec![WordSgmt::Lit("foo}".into())]
        );

        // We can have nested dollar expressions.
        assert_parse!(
            param_expansion("${foo$(bar)}"),
            vec![
                WordSgmt::Lit("foo".into()),
                DollarExp::CmdSub(Some(tests::pipeline("bar").into())).into()
            ]
        );

        // Can be empty.
        assert_parse!(param_expansion("${}"), vec![]);
    }
}
