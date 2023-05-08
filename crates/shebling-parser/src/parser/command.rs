use once_cell::sync::Lazy;
use std::collections::HashSet;

use super::*;
use crate::{location::Location, ParseContext};
use expansion::EXTGLOB_PREFIX;
use quoted::DOUBLE_ESCAPABLE;

const UNIDASHES: &str =
    "\u{058A}\u{05BE}\u{2010}\u{2011}\u{2012}\u{2013}\u{2014}\u{2015}\u{FE63}\u{FF0D}";

// TODO: Use the standard library's once_cell when it's stable.
static COMMON_COMMANDS: Lazy<HashSet<&'static str>> = Lazy::new(|| {
    // Taken from https://github.com/koalaman/shellcheck/blob/b1ca3929e387446f3e3db023d716cf3787370437/src/ShellCheck/Data.hs#L96.
    HashSet::from_iter([
        "admin",
        "alias",
        "ar",
        "asa",
        "at",
        "awk",
        "basename",
        "batch",
        "bc",
        "bg",
        "break",
        "c99",
        "cal",
        "cat",
        "cd",
        "cflow",
        "chgrp",
        "chmod",
        "chown",
        "cksum",
        "cmp",
        "colon",
        "comm",
        "command",
        "compress",
        "continue",
        "cp",
        "crontab",
        "csplit",
        "ctags",
        "cut",
        "cxref",
        "date",
        "dd",
        "delta",
        "df",
        "diff",
        "dirname",
        "dot",
        "du",
        "echo",
        "ed",
        "env",
        "eval",
        "ex",
        "exec",
        "exit",
        "expand",
        "export",
        "expr",
        "fc",
        "fg",
        "file",
        "find",
        "fold",
        "fort77",
        "fuser",
        "gencat",
        "get",
        "getconf",
        "getopts",
        "grep",
        "hash",
        "head",
        "iconv",
        "ipcrm",
        "ipcs",
        "jobs",
        "join",
        "kill",
        "lex",
        "link",
        "ln",
        "locale",
        "localedef",
        "logger",
        "logname",
        "lp",
        "ls",
        "m4",
        "mailx",
        "make",
        "man",
        "mesg",
        "mkdir",
        "mkfifo",
        "more",
        "mv",
        "newgrp",
        "nice",
        "nl",
        "nm",
        "nohup",
        "od",
        "paste",
        "patch",
        "pathchk",
        "pax",
        "pr",
        "printf",
        "prs",
        "ps",
        "pwd",
        "qalter",
        "qdel",
        "qhold",
        "qmove",
        "qmsg",
        "qrerun",
        "qrls",
        "qselect",
        "qsig",
        "qstat",
        "qsub",
        "read",
        "readonly",
        "renice",
        "return",
        "rm",
        "rmdel",
        "rmdir",
        "sact",
        "sccs",
        "sed",
        "set",
        "sh",
        "shift",
        "sleep",
        "sort",
        "split",
        "strings",
        "strip",
        "stty",
        "tabs",
        "tail",
        "talk",
        "tee",
        "test",
        "time",
        "times",
        "touch",
        "tput",
        "tr",
        "trap",
        "tsort",
        "tty",
        "type",
        "ulimit",
        "umask",
        "unalias",
        "uname",
        "uncompress",
        "unexpand",
        "unget",
        "uniq",
        "unlink",
        "unset",
        "uucp",
        "uudecode",
        "uuencode",
        "uustat",
        "uux",
        "val",
        "vi",
        "wait",
        "wc",
        "what",
        "who",
        "write",
        "xargs",
        "yacc",
        "zcat",
    ])
});

// TODO: Divide these into separate modules.

fn bats_test(span: Span) -> ParseResult<BatsTest> {
    // Read the test header.
    let (span, mut header) = preceded(
        pair(tag("@test "), trivia),
        peek(recognize_string(is_not("\n"))),
    )(span)?;

    // The test name is everything before the last ' {'.
    if let Some(header_len) = header.rfind(" {") {
        header.truncate(header_len);
    } else {
        return context("invalid test name!", fail)(span);
    }

    // Parse the test body defined after the truncated header.
    let (span, body) = preceded(
        pair(take(header.len()), trivia),
        context("invalid test body!", brace_group),
    )(span)?;

    Ok((span, BatsTest::new(header.trim_end(), body)))
}

fn brace_group(span: Span) -> ParseResult<Term> {
    // Check spacing after the opening brace, but note that '{(' is legal.
    let (span, (start, space, paren)) = tuple((
        terminated(position, char('{')),
        multi_trivia,
        followed_by(char('(')),
    ))(span)?;
    if space.is_empty() && !paren {
        span.extra
            .diag(ParseDiagnostic::builder(ParseDiagnosticKind::MissingSpace).range(&span));
    }

    // Check for empty groups.
    let (span, closing_brace) = opt(peek(preceded(char('}'), position)))(span)?;
    if let Some(end) = closing_brace {
        span.extra.diag(
            ParseDiagnostic::builder(ParseDiagnosticKind::SusToken)
                .label("empty code block", Range::new(start, end)),
        );
    }

    // Parse the braced term.
    let (span, term) = term(span)?;

    // Check the closing brace.
    let (span, _) = terminated(
        context(
            "missing the closing brace (or the semicolon or newline before it)",
            char('}'),
        ),
        trivia,
    )(span)?;

    Ok((span, term))
}

fn cmd(span: Span) -> ParseResult<Cmd> {
    alt((
        into(compound_cmd),
        into(cond_cmd),
        into(coproc),
        into(simple_cmd),
    ))(span)
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
            // TODO map(if_cmd, Construct::If),
            // TODO map(for_loop, Construct::ForLoop),
            // TODO preceded(token(Keyword::Select), map(in_listed, Construct::Select)),
            // TODO map(case_cmd, Construct::Case),
            map(bats_test, Construct::BatsTest),
            // TODO map(function, Construct::Function),
        )),
        many0(redir),
    )(span)?;

    // Check for post-command gibberish.
    let (span, trailing) = opt(peek(preceded(
        not(alt((peek_sus_token, swallow(peek(char('{')))))),
        ranged(many1(word)),
    )))(span)?;
    if let Some((_, range)) = trailing {
        span.extra.diag(
            ParseDiagnostic::builder(ParseDiagnosticKind::SusToken)
                .label("trailing words after compound command", range)
                .help("You might have bad redirections or a missing operator."),
        );
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
                    "expected another argument for the binary expression!",
                    // The 2nd argument is parsed separately if the operator is =~.
                    |span| {
                        if op == BinOp::Match {
                            map(many1(regex_sgmts), |sgmts| {
                                let sgmts = sgmts.into_iter().flatten().collect::<Vec<_>>();
                                Word::new(sgmts)
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
            if let Some((_, range)) = bracket {
                let (test, group) = if single_bracketed {
                    ("[ .. ]", "\\( .. \\)")
                } else {
                    ("[[ .. ]]", "( .. )")
                };
                span.extra.diag(
                    ParseDiagnostic::builder(ParseDiagnosticKind::SusToken)
                        .range(range)
                        .help(format!(
                            "If grouping expressions inside {}, use {}.",
                            test, group
                        )),
                );
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
                span.extra
                    .diag(ParseDiagnostic::builder(ParseDiagnosticKind::MissingSpace).range(range));
            }
            if single_bracketed && space.contains('\n') {
                span.extra.diag(
                    ParseDiagnostic::builder(ParseDiagnosticKind::MissingEscape)
                        .label("you need to escape line breaks inside [ .. ]", range),
                );
            }

            Ok((span, space))
        }
    }

    fn cond_word(span: Span) -> ParseResult<Word> {
        // Peek a misplaced bracket.
        let (span, _) = context("unexpected bracket!", not(pair(trivia, char(']'))))(span)?;

        // Parse the word.
        let (span, word) = context("invalid condition argument!", word)(span)?;

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
                span.extra.diag(
                    ParseDiagnostic::builder(ParseDiagnosticKind::MissingSpace)
                        .range(Location::from(&span).translate(-1)),
                );

                return fail(span);
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
                    span.extra.diag(
                        ParseDiagnostic::builder(ParseDiagnosticKind::NotShellCode).label(
                            "[ is a command, not a syntax marker for the if statement!",
                            range,
                        ),
                    );
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
            let (span, (unidash, range)) = ranged(one_of(UNIDASHES))(span)?;
            span.extra.diag(
                ParseDiagnostic::builder(ParseDiagnosticKind::Unichar).label("unicode dash", range),
            );

            Ok((span, unidash))
        }))(span)?;

        // Parse the flag.
        let (span, flag) = cut(context("expected a test operator!", alpha1))(span)?;

        Ok((span, CondOp::new(format!("-{}", flag))))
    }

    fn group<'a>(single_bracketed: bool) -> impl FnMut(Span<'a>) -> ParseResult<CondGroup> {
        let paren = |p| {
            move |span| {
                let (span, ((_, escaped), range)) = terminated(
                    ranged(escaped_or_quoted(char(p))),
                    cond_space(single_bracketed, single_bracketed),
                )(span)?;
                // Parentheses should be escaped inside [ .. ].
                if single_bracketed && !escaped {
                    span.extra.diag(
                        ParseDiagnostic::builder(ParseDiagnosticKind::MissingEscape)
                            .range(range)
                            .help("You should use \\( and \\) inside [ .. ], or combine [ .. ] expressions."),
                    )
                }

                // Parentheses should not be escaped inside [[ .. ]].
                if !single_bracketed && escaped {
                    span.extra.diag(
                        ParseDiagnostic::builder(ParseDiagnosticKind::BadEscape)
                            .label(format!("no need to escape {} inside [[ .. ]].", p), range),
                    );
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
            // Unflagged logic operator (e.g. &&).
            terminated(
                map(token(unflagged_op), |op| CondOp::new(op.token())),
                cond_space(false, single_bracketed),
            ),
            // Flagged logic operator (e.g. -a).
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
        let (span, (expr, expr_end)) =
            terminated(pair(cond_word, position), pair(trivia, peek_arith))(span)?;

        // Check if the expression ends with what looks like a binary operator. If
        // so, the user might have forgotten a space.
        if let Some(WordSgmt::Lit(lit)) = expr.sgmts().last() {
            let lit = lit.value();
            // TODO: create a shared public array with these?
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
                span.extra.diag(
                    ParseDiagnostic::builder(ParseDiagnosticKind::MissingSpace).range(
                        Location::from(expr_end).translate((op.len() as isize).wrapping_neg()),
                    ),
                );
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
            position,
            alt((
                preceded(one_of("+*/%"), position),
                delimited(char('-'), position, char(' ')),
            )),
        )))(span)?;

        // Tests should do math stuff inside $((..)).
        if let Some((start, end)) = math_following {
            span.extra.diag(
                ParseDiagnostic::builder(ParseDiagnosticKind::SusToken)
                    .label("math operator!", Range::new(start, end))
                    .help("Inside conditions, do math stuff inside $(( .. ))."),
            )
        }

        Ok((span, ()))
    }

    fn regex_sgmts(span: Span) -> ParseResult<Vec<WordSgmt>> {
        alt((
            // Read a group.
            map(
                tuple((
                    into(lit(char('('))),
                    many0(alt((
                        regex_sgmts,
                        map(
                            many1(alt((
                                escaped(""),
                                recognize_string(is_not(&*format!("'(){}", DOUBLE_ESCAPABLE))),
                            ))),
                            |lits| vec![Lit::new(lits.concat()).into()],
                        ),
                    ))),
                    into(lit(char(')'))),
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
                    into(lit(one_of(&*format!("{}{{}}[]|$", EXTGLOB_PREFIX)))),
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
                    "expected an operand for the unary expression!",
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
        (span, Cond::empty(single_bracketed))
    } else {
        map(content(single_bracketed), |expr| {
            Cond::with_expr(single_bracketed, expr)
        })(span)?
    };

    // Verify the closing bracket.
    let (span, (second_bracket, bracket_range)) = terminated(
        context(
            "expected the test to end here (don't wrap commands in square brackets!)",
            ranged(preceded(char(']'), opt(char(']')))),
        ),
        trivia,
    )(span)?;

    // Make sure the brackets match.
    if (single_bracketed && second_bracket.is_some())
        || (!single_bracketed && second_bracket.is_none())
    {
        span.extra.diag(
            ParseDiagnostic::builder(ParseDiagnosticKind::SusToken).label(
                "closing bracket doesn't match the opening one",
                bracket_range,
            ),
        );
    }

    Ok((span, cond))
}

fn cond_block<'a>(keyword: Keyword) -> impl FnMut(Span<'a>) -> ParseResult<CondBlock> {
    preceded(
        pair(token(keyword), trivia),
        map(pair(term, do_group), |(cond, block)| {
            CondBlock::new(cond, block)
        }),
    )
}

fn cond_cmd(span: Span) -> ParseResult<CondCmd> {
    // Read the condition and suffix redirections.
    let (span, (cond, redirs)) = pair(cond, many0(redir))(span)?;

    // || or && should be used between test conditions.
    let (span, cond_op) = opt(peek(ranged(consumed(alt((
        value(ControlOp::AndIf, alt((tag("-a"), tag("and")))),
        value(ControlOp::OrIf, alt((tag("-o"), tag("or")))),
    ))))))(span)?;
    if let Some(((bad_op, good_op), range)) = &cond_op {
        span.extra.diag(
            ParseDiagnostic::builder(ParseDiagnosticKind::BadOperator)
                .range(*range)
                .help(format!(
                    "Use {} instead of {} between test commands.",
                    good_op.token(),
                    bad_op
                )),
        );
    }

    // Check if there's a trailing word that's not keyword/operator-like.
    let (span, (sus_token, next_word)) = pair(opt(peek_sus_token), opt(peek(ranged(word))))(span)?;
    if let Some((_, range)) = next_word {
        if sus_token.is_none() && cond_op.is_none() {
            span.extra.diag(
                ParseDiagnostic::builder(ParseDiagnosticKind::SusToken)
                    .label("unexpected parameter after condition", range)
                    .help("You might be missing a && or ||."),
            )
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

fn do_group(span: Span) -> ParseResult<Term> {
    // 'do' should come before the 'done'.
    let (span, _) = context(
        "did you forget the 'do' for this loop?",
        not(token(Keyword::Done)),
    )(span)?;

    // Parse the 'do'.
    let (span, _) = terminated(context("expected 'do'!", token(Keyword::Do)), trivia)(span)?;

    // Warn about 'do;'s.
    let (span, semi) = terminated(opt(ranged(token(ControlOp::Semi))), multi_trivia)(span)?;
    if let Some((_, range)) = semi {
        span.extra.diag(
            ParseDiagnostic::builder(ParseDiagnosticKind::BadOperator)
                .label("semicolon after 'do'", range)
                .help("Just delete it."),
        );
    }

    // Parse the term, bailing if the clause is empty.
    let (span, term) = preceded(
        context("empty 'do' clause!", not(token(Keyword::Done))),
        term,
    )(span)?;

    // Parse the 'done'.
    let (span, _) = terminated(context("expected 'done'!", token(Keyword::Done)), trivia)(span)?;

    // If we peek the beginning of a process substitution, the user might have missed a '<'.
    let (span, proc_sub_start) = opt(peek(ranged(tag("<("))))(span)?;
    if let Some((_, range)) = proc_sub_start {
        span.extra.diag(
            ParseDiagnostic::builder(ParseDiagnosticKind::SusToken)
                .label("process substitution?", range)
                .help("For redirection, use 'done < <(cmd)' (you're missing one '<')."),
        );
    }

    Ok((span, term))
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

fn pipeline(span: Span) -> ParseResult<Pipeline> {
    // Ensure there's not a misplaced token.
    let (span, _) = context("unexpected token", not(peek_sus_token))(span)?;

    // Parse a bang if there's one.
    let (span, _) = opt(|span| {
        let (span, trivia) = preceded(char('!'), trivia)(span)?;
        if trivia.is_empty() {
            span.extra
                .diag(ParseDiagnostic::builder(ParseDiagnosticKind::MissingSpace).range(&span));
        }

        Ok((span, ()))
    })(span)?;

    // Parse the commands.
    let (span, cmds) = separated_list1(
        context(
            "invalid pipeline operator!",
            delimited(
                // Make sure we match | and |& but not ||.
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

fn simple_cmd(span: Span) -> ParseResult<SimpleCmd> {
    fn let_seq(span: Span) -> ParseResult<CmdSuffixSgmt> {
        // Read the input math word.
        let (span, (mut start, seq_span)) =
            pair(position, terminated(recognize(word), trivia))(span)?;

        // Remove quotes.
        let (mut seq_span, quote) = opt(one_of("'\""))(seq_span)?;
        if let Some(quote) = quote {
            (seq_span, start) = position(seq_span)?;
            (_, seq_span) =
                all_consuming(terminated(take_till(|c| c == quote), char(quote)))(seq_span)?;
        }

        // Subparse the math expression.
        let seq_span = unsafe {
            Span::new_from_raw_offset(
                start.location_offset(),
                start.location_line(),
                &seq_span,
                ParseContext::new(),
            )
        };
        let (seq_span, seq) = all_consuming(arith_seq)(seq_span)?;
        span.extra.extend_diags(seq_span.extra);

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
            if lit.value().starts_with("//") || {
                if let Some(WordSgmt::Glob(glob)) = &cmd.sgmts().get(1) {
                    lit.value() == "/" && glob == "*"
                } else {
                    false
                }
            } {
                span.extra.diag(
                    ParseDiagnostic::builder(ParseDiagnosticKind::NotShellCode)
                        .label("C-like comment", *range)
                        .help("In shell scripts, comments begin with #."),
                );
            }
        }

        if let Some(cmd) = cmd.as_lit() {
            let cmd = cmd.value();

            // Not using the right keyword for an "else if" statement.
            if matches!(&*cmd.to_ascii_lowercase(), "elsif" | "elseif") {
                span.extra.diag(
                    ParseDiagnostic::builder(ParseDiagnosticKind::NotShellCode)
                        .range(*range)
                        .help("Use elif to start another branch."),
                );
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
                        "quote or escape your parens when using eval!",
                        preceded(char('('), fail),
                    )),
                )))(span)?,
                "let" => context(
                    "expected an expression for this let command!",
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

fn subshell(span: Span) -> ParseResult<Term> {
    delimited(
        // Opening parenthesis and whitespace:
        pair(char('('), multi_trivia),
        // The subshell's list:
        term,
        // Closing parenthesis and whitespace:
        tuple((
            multi_trivia,
            context("expected a closing )!", char(')')),
            trivia,
        )),
    )(span)
}

pub(super) fn term(span: Span) -> ParseResult<Term> {
    fn and_or(span: Span) -> ParseResult<Term> {
        // Left-fold the pipelines into a list.
        preceded(
            multi_trivia,
            map(
                pair(
                    map(pipeline, Term::Pipeline),
                    many0(separated_pair(
                        alt((token(ControlOp::AndIf), token(ControlOp::OrIf))),
                        pair(trivia, linebreak),
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
        let (span, (start, and)) = pair(position, token(ControlOp::And))(span)?;

        // Warn if the & seems to begin an HTML entity.
        let (mut span, entity_end) = opt(peek(preceded(
            pair(
                alt((
                    preceded(char('#'), digit1),
                    preceded(tag("#x"), alphanumeric1),
                    alpha1,
                )),
                char(';'),
            ),
            position,
        )))(span)?;
        if let Some(entity_end) = entity_end {
            span.extra.diag(
                ParseDiagnostic::builder(ParseDiagnosticKind::SusToken)
                    .label("unquoted HTML entity", Range::new(&start, entity_end))
                    .help("Replace it with the corresponding character."),
            );
        } else {
            // Warn if the & might be ending a command by mistake (e.g. it is in a URL).
            let alpha_char;
            (span, alpha_char) =
                followed_by(satisfy(|c| c == '_' || c.is_ascii_alphabetic()))(span)?;
            if alpha_char {
                span.extra.diag(
                    ParseDiagnostic::builder(ParseDiagnosticKind::MissingSpace)
                        .label("this & terminates the command", Range::new(&start, &span))
                        .help("Quote or escape the & if you want a literal ampersand, or add a space after it."),
                );
            }
        }

        // Warn about redundant semicolons.
        let (span, semi_end) =
            preceded(trivia, opt(preceded(token(ControlOp::Semi), position)))(span)?;
        if let Some(semi_end) = semi_end {
            span.extra.diag(
                ParseDiagnostic::builder(ParseDiagnosticKind::SusToken)
                    .label(
                        "both & and ; terminate the command",
                        Range::new(start, semi_end),
                    )
                    .help("Just use one of them."),
            );
        }

        Ok((span, and))
    }

    fn sep(span: Span) -> ParseResult<ControlOp> {
        alt((
            terminated(
                alt((
                    // &, but not && or &>.
                    terminated(and_sep, not(one_of("&>"))),
                    // ;, but not clause operators.
                    terminated(token(ControlOp::Semi), not(one_of(";&"))),
                )),
                pair(trivia, linebreak),
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::tests;

    #[test]
    fn test_bats_test() {
        // These tests are the ones that ShellCheck uses because idek what a bats test is.
        assert_parse!(
            bats_test("@test 'can parse' {\n  true\n}") => "",
            BatsTest::new("'can parse'", tests::lit_pipeline("true"))
        );
        assert_parse!(
            bats_test("@test random text !(@*$Y&! {\n  true\n}") => "",
            BatsTest::new("random text !(@*$Y&!", tests::lit_pipeline("true"))
        );
        assert_parse!(
            bats_test("@test foo { bar { baz {\n  true\n}") => "",
            BatsTest::new("foo { bar { baz", tests::lit_pipeline("true"))
        );
        assert_parse!(bats_test("@test foo \n{\n true\n}") => Err(
            (1, 7),
            Notes: [((1, 7), "invalid test name")]
        ));
    }

    #[test]
    fn test_brace_group() {
        // Space after the opening brace is required except if we have {(.
        assert_parse!(
            brace_group("{foo;}") => "",
            Term::from(tests::lit_pipeline("foo")),
            [((1, 2), ParseDiagnosticKind::MissingSpace)]
        );

        // TODO: Add a test for {()}
        // The space before the } can be omitted if there's a semicolon.
        assert_parse!(brace_group("{ foo;}") => "", Term::from(tests::lit_pipeline("foo")));

        // Empty code blocks aren't allowed.
        assert_parse!(brace_group("{ }") => Err(
            (1, 3),
            Notes: [((1, 3), "unexpected token")],
            Diags: [((1, 1), (1, 4), ParseDiagnosticKind::SusToken)]
        ));

        // The closing curly needs a semicolon or newline before it.
        assert_parse!(brace_group("{ foo}") => Err(
            (1, 7),
            Notes: [((1, 7), "missing the closing brace")],
            Diags: [((1, 6), (1, 7), ParseDiagnosticKind::SusToken)]
        ));
    }

    #[test]
    fn test_compound_command() {
        // TODO
    }

    #[test]
    fn test_cond() {
        // Operators can be escaped or quoted.
        assert_parse!(
            cond("[ foo \\< bar ]") => "",
            Cond::with_expr(
                true,
                CondBinExpr::new(tests::lit_word("foo"), tests::lit_word("bar"), BinOp::Lt),
            )
        );
        assert_parse!(
            cond("[ foo '<' bar ]") => "",
            Cond::with_expr(
                true,
                CondBinExpr::new(tests::lit_word("foo"), tests::lit_word("bar"), BinOp::Lt),
            )
        );

        // Same with parentheses.
        assert_parse!(
            cond("[ '(' foo \\) ]") => "",
            Cond::with_expr(true, CondGroup::new(tests::lit_word("foo")))
        );
        // ...In fact, you have to do it inside single brackets.
        assert_parse!(
            cond("[ '(' foo ) ]") => "",
            Cond::with_expr(true, CondGroup::new(tests::lit_word("foo"))),
            [((1, 11), (1, 12), ParseDiagnosticKind::MissingEscape)]
        );
        // ...But in double brackets is wrong to do it.
        assert_parse!(
            cond("[[ '(' foo ) ]]") => "",
            Cond::with_expr(false, CondGroup::new(tests::lit_word("foo"))),
            [((1, 4), (1, 7), ParseDiagnosticKind::BadEscape)]
        );

        // New lines have to be escaped inside single brackets.
        assert_parse!(cond("[\n]") => "", Cond::empty(true), [((1, 2), (2, 1), ParseDiagnosticKind::MissingEscape)]);

        // Empty conditions are legal.
        assert_parse!(cond("[[ ]]") => "", Cond::empty(false));

        // Bracket mismatch.
        assert_parse!(
            cond("[ ]]") => "",
            Cond::empty(true),
            [((1, 3), (1, 5), ParseDiagnosticKind::SusToken)]
        );

        // if [ grep foo file ] pitfall.
        assert_parse!(cond("[ grep foo file ]") => Err(
            (1, 8),
            Notes: [((1, 8), "expected the test to end")],
            Diags: [((1, 3), (1, 7), ParseDiagnosticKind::NotShellCode)]
        ));

        // Incorrect opening bracket.
        assert_parse!(cond("]") => Err(1, 1));

        // Missing the closing bracket.
        assert_parse!(cond("[ ") => Err(
            (1, 3),
            Notes: [((1, 3), "expected a non-empty word"), ((1, 3), "invalid condition argument")]
        ));

        // Missing space after the opening bracket.
        assert_parse!(cond("[]") => Err(
            (1, 2),
            Notes: [((1, 2), "unexpected bracket")],
            Diags: [((1, 2), ParseDiagnosticKind::MissingSpace)]
        ));

        // Should use parens for grouping instead of [].
        assert_parse!(cond("[ [ foo ] ]") => Err(
            (1, 5),
            Notes: [((1, 5), "expected the test to end")],
            Diags: [((1, 3), (1, 4), ParseDiagnosticKind::SusToken)]
        ));

        // Shouldn't use math operators inside test conditions.
        assert_parse!(cond("[ x + 1 ]") => Err(
            (1, 5),
            Notes: [((1, 5), "expected the test to end")],
            Diags: [((1, 5), (1, 6), ParseDiagnosticKind::SusToken)]
        ));
        assert_parse!(cond("[ x - 1 ]") => Err(
            (1, 6),
            Notes: [((1, 6), "expected a test operator")],
            Diags: [((1, 5), (1, 6), ParseDiagnosticKind::SusToken)]
        ));

        // Missing spaces around the operator.
        assert_parse!(cond("[ foo= bar ]") => Err(
            (1, 8),
            Notes: [((1, 8), "expected the test to end")],
            Diags: [((1, 6), ParseDiagnosticKind::MissingSpace)]
        ));
        assert_parse!(cond("[ foo\\>= bar ]") => Err(
            (1, 10),
            Notes: [((1, 10), "expected the test to end")],
            Diags: [((1, 7), ParseDiagnosticKind::MissingSpace)]
        ));

        // Word that prematurely ends the condition.
        assert_parse!(cond("[ foo] ]") => Err(
            (1, 7),
            Diags: [((1, 6), ParseDiagnosticKind::MissingSpace)]
        ));
    }

    #[test]
    fn test_cond_cmd() {
        // Valid condition commands.
        assert_parse!(
            cond_cmd("[[ foo ]]") => "",
            CondCmd::new(Cond::with_expr(false, tests::lit_word("foo")), vec![])
        );
        assert_parse!(
            cond_cmd("[ foo ] > bar") => "",
            CondCmd::new(
                Cond::with_expr(true, tests::lit_word("foo")),
                vec![Redir::new(None, RedirOp::Great, tests::lit_word("bar"))]
            )
        );

        // Wrong operator between conditions.
        assert_parse!(
            cond_cmd("[ foo ] -a [ bar ]") => "-a [ bar ]",
            CondCmd::new(Cond::with_expr(true, tests::lit_word("foo")), vec![]),
            [((1, 9), (1, 11), ParseDiagnosticKind::BadOperator)]
        );

        // Missing the operator between conditions.
        assert_parse!(
            cond_cmd("[ foo ] [ bar ]") => "[ bar ]",
            CondCmd::new(
                Cond::with_expr(true, tests::lit_word("foo")),
                vec![],
            ),
            [((1, 9), (1, 10), ParseDiagnosticKind::SusToken)]
        );
    }

    #[test]
    fn test_coproc() {
        // TODO
    }

    #[test]
    fn test_do_group() {
        // TODO
    }

    #[test]
    fn test_pipeline() {
        // A single command is a valid pipeline.
        assert_parse!(
            pipeline("! ls") => "",
            Pipeline::new(vec![tests::lit_cmd("ls").into()])
        );

        // Make sure we can handle new lines between commands.
        assert_parse!(
            pipeline("ls |\n\ngrep .txt") => "",
            Pipeline::new(
                vec![
                    tests::lit_cmd("ls").into(),
                    SimpleCmd::new(
                        Some(tests::lit_word("grep")),
                        vec![],
                        vec![tests::lit_word(".txt").into()],
                    ).into()
                ]
            )
        );

        // Pipelines aren't separated with ||.
        assert_parse!(pipeline("ls || grep .txt") => "|| grep .txt", tests::lit_pipeline("ls"));

        // Error when the pipeline begins with a sus keyword.
        assert_parse!(pipeline("then") => Err((1, 1), Notes: [((1, 1), "unexpected")]));
    }

    #[test]
    fn test_simple_cmd() {
        // Ignore backslashes for alias supressions.
        assert_parse!(simple_cmd("\\ls") => "", tests::lit_cmd("ls"));

        // Can handle just a prefix.
        assert_parse!(
            simple_cmd("foo=bar") => "",
            SimpleCmd::new(
                None,
                vec![Assign::new(Variable::new("foo"), tests::lit_word("bar"), BinOp::Eq).into()],
                vec![],
            )
        );

        // Prefixes can contain assignments or redirections.
        assert_parse!(
            simple_cmd("foo=bar >file ls") => "",
            SimpleCmd::new(
                Some(tests::lit_word("ls")),
                vec![
                    Assign::new(Variable::new("foo"), tests::lit_word("bar"), BinOp::Eq).into(),
                    Redir::new(None, RedirOp::Great, tests::lit_word("file")).into(),
                ],
                vec![],
            )
        );

        // Warn when the command looks like a C-like comment.
        assert_parse!(
            simple_cmd("// echo") => "",
            SimpleCmd::new(
                Some(tests::lit_word("//")),
                vec![],
                vec![tests::lit_word("echo").into()]
            ),
            [((1, 1), (1, 3), ParseDiagnosticKind::NotShellCode)]
        );
        assert_parse!(
            simple_cmd("/* echo */") => "echo */",
            SimpleCmd::new(
                Some(Word::new(vec![Lit::new("/").into(), Glob::new("*").into()])),
                vec![],
                vec![]
            ),
            [((1, 1), (1, 3), ParseDiagnosticKind::NotShellCode)]
        );

        // Not using the right "else if" keyword.
        assert_parse!(
            simple_cmd("elseif") => "",
            tests::lit_cmd("elseif"),
            [((1, 1), (1, 7), ParseDiagnosticKind::NotShellCode)]
        );

        // let expressions (which can be quoted or not).
        assert_parse!(
            simple_cmd("let x=0") => "",
            SimpleCmd::new(
                Some(tests::lit_word("let")),
                vec![],
                vec![ArithSeq::new(vec![
                    ArithBinExpr::new(
                        Variable::new("x"),
                        tests::arith_number("0"),
                        BinOp::Eq
                    ).into()
                ]).into()]
            )
        );
        assert_parse!(
            simple_cmd("let \"x = 0\"") => "",
            SimpleCmd::new(
                Some(tests::lit_word("let")),
                vec![],
                vec![ArithSeq::new(vec![
                    ArithBinExpr::new(
                        Variable::new("x"),
                        tests::arith_number("0"),
                        BinOp::Eq
                    ).into()
                ]).into()]
            )
        );

        // let expressions need arguments.
        assert_parse!(simple_cmd("let") => Err(
            (1, 4),
            Notes: [((1, 4), "expected a non-empty word"), ((1, 4), "expected an expression")]
        ));
    }

    #[test]
    fn test_subshell() {
        // Since parentheses are operators, they don't need to be separated
        // from the list by whitespace.
        assert_parse!(subshell("(foo)") => "", Term::from(tests::lit_pipeline("foo")));
    }

    #[test]
    fn test_term() {
        // Handle line continuations and lists of lists.
        assert_parse!(
            term("ls;\\\necho 'foo' && echo 'bar'") => "",
            List::new(
                tests::lit_pipeline("ls"),
                List::new(
                    Pipeline::new(
                        vec![
                            SimpleCmd::new(
                                Some(tests::lit_word("echo")),
                                vec![],
                                vec![Word::new(vec![SingleQuoted::new("foo").into()]).into()]
                            ).into()
                        ]
                    ),
                    Pipeline::new(
                        vec![
                            SimpleCmd::new(
                                Some(tests::lit_word("echo")),
                                vec![],
                                vec![Word::new(vec![SingleQuoted::new("bar").into()]).into()]
                            ).into()
                        ]
                    ),
                    ControlOp::AndIf,
                ),
                ControlOp::Semi,
            ).into()
        );

        // Warn when there's an HTML entity.
        assert_parse!(
            term("foo &amp;&amp; bar") => ";&amp; bar",
            List::new(
                tests::lit_pipeline("foo"),
                tests::lit_pipeline("amp"),
                ControlOp::And
            ).into(),
            [((1, 5), (1, 10), ParseDiagnosticKind::SusToken)]
        );

        // Check if something looks like URL query parameters.
        assert_parse!(
            term("com&q=foo") => "",
            List::new(
                tests::lit_pipeline("com"),
                Pipeline::new(
                    vec![
                        SimpleCmd::new(
                            None,
                            vec![
                                Assign::new(
                                    Variable::new("q"),
                                    tests::lit_word("foo"),
                                    BinOp::Eq
                                ).into()
                            ],
                            vec![],
                        ).into()
                    ]
                ),
                ControlOp::And,
            ).into(),
            [((1, 4), (1, 5), ParseDiagnosticKind::MissingSpace)]
        );
    }
}
