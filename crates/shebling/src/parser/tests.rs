use super::*;
use nom::Finish;

macro_rules! located {
    ($l:literal, $c:literal) => {
        crate::ast::Location::new($l, $c)
    };

    (($l:literal, $c:literal), $lint:ident) => {
        located!(($l, $c), ($l, $c), $lint)
    };

    (($l1:literal, $c1:literal), ($l2:literal, $c2:literal), $lint:ident) => {{
        (
            crate::ast::Range::new(located!($l1, $c1), located!($l2, $c2)),
            stringify!($lint),
        )
    }};
}

macro_rules! assert_eq {
    ($parsed:expr => $rem:literal, $res:expr) => { assert_eq!($parsed => $rem, $res, []); };

    (
        $parsed:expr => $rem:literal,
        $res:expr,
        [$((($l1:literal, $c1:literal), $(($l2:literal, $c2:literal),)? $lint:ident)),*]
    ) => {
        ::pretty_assertions::assert_eq!(
            $parsed,
            Ok((
                $rem,
                $res.into(),
                vec![$(located!(($l1, $c1), $(($l2, $c2),)? $lint)),*]
            ))
        );
    };

    ($parsed:expr => Err($l:literal, $c:literal)) => {
        assert_eq!($parsed => Err(($l, $c), Lints: [], Notes: []));
    };

    ($parsed:expr => Err(
        ($l:literal, $c:literal),
        Lints: [$((($l1:literal, $c1:literal), $(($l2:literal, $c2:literal),)? $lint:ident)),+]
    )) => {
        assert_eq!($parsed => Err(($l, $c), Lints: [$((($l1, $c1), $(($l2, $c2),)? $lint)),+], Notes: []));
    };

    ($parsed:expr => Err(
        ($l:literal, $c:literal),
        Notes: [$((($l1:literal, $c1:literal), $note:literal)),+]
    )) => {
        assert_eq!($parsed => Err(($l, $c), Lints: [], Notes: [$((($l1, $c1), $note)),+]));
    };

    (
        $parsed:expr => Err(
            ($l:literal, $c:literal),
            Lints: [$((($l1:literal, $c1:literal), $(($l2:literal, $c2:literal),)? $lint:ident)),*],
            Notes: [$((($l3:literal, $c3:literal), $note:literal)),*]
        )
    ) => {
        let (location, lints, notes) = $parsed.expect_err("Test should have failed.");

        // Check the location.
        ::pretty_assertions::assert_eq!(location, located!($l, $c));

        // Check the lints.
        ::pretty_assertions::assert_eq!(
            lints,
            vec![$(located!(($l1, $c1), $(($l2, $c2),)? $lint)),*]
        );

        // For flexibility, just check that the notes have the expected messages.
        let mut notes = notes.into_iter();
        $(
            let (location, actual) = notes.next().expect("Expected a parser note.");
            ::pretty_assertions::assert_eq!(location, located!($l3, $c3));
            assert!(actual.contains($note), "Expected \"{}\" to contain \"{}\"", actual, $note);
        )*
        let last_note = notes.next();
        assert!(last_note.is_none(), "There's a note left: {:#?}", last_note);
    };
}

#[test]
fn test_arith_seq() {
    let parse = summarize(arith_seq);

    // A single digit is fine.
    assert_eq!(parse("0") => "", ArithSeq::new(vec![arith_number("0").into()]));
    assert_eq!(
        parse("!!0") => "",
        ArithSeq::new(vec![
            ArithUnExpr::new(
                ArithUnExpr::new(arith_number("0"), UnOp::Not),
                UnOp::Not,
            ).into()
        ])
    );
    // Pre and post increments and decrements.
    assert_eq!(
        parse("x++ + --y") => "",
        ArithSeq::new(vec![
            ArithBinExpr::new(
                ArithUnExpr::new(Variable::new("x"), UnOp::Inc),
                ArithUnExpr::new(Variable::new("y"), UnOp::Dec),
                BinOp::Add
            ).into()
        ])
    );
    // Trinary conditions with weird spacing.
    assert_eq!(
        parse("a?\\\nb:c?\td : e") => "",
        ArithSeq::new(vec![
            ArithTriExpr::new(
                Variable::new("a"),
                Variable::new("b"),
                ArithTriExpr::new(Variable::new("c"), Variable::new("d"), Variable::new("e")),
            ).into()
        ])
    );
    // Groups and dollar expressions.
    assert_eq!(
        parse("($x ** 2)") => "",
        ArithSeq::new(vec![
            ArithGroup::new(ArithSeq::new(vec![
                ArithBinExpr::new(
                    ArithExpansion::new(vec![
                        DollarExp::Variable(Variable::new("x")).into(),
                    ]),
                    arith_number("2"),
                    BinOp::Pow,
                ).into(),
            ])).into()
        ])
    );
    // A list of assignments.
    assert_eq!(
        parse("x+=1, y<<=2") => "",
        ArithSeq::new(vec![
            ArithBinExpr::new(Variable::new("x"), arith_number("1"), BinOp::AddEq).into(),
            ArithBinExpr::new(Variable::new("y"), arith_number("2"), BinOp::ShlEq).into()
        ])
    );
    // Lint for using a test operator inside math stuff.
    assert_eq!(
        parse("x -lt") => "",
        ArithSeq::new(vec![
            ArithBinExpr::new(Variable::new("x"), Variable::new("lt"), BinOp::Sub).into()
        ]),
        [((1, 3), (1, 6), COMPARATOR_IN_MATH)]
    );
}

#[test]
fn test_assign() {
    let parse = summarize(assign);

    // Legit assignments.
    assert_eq!(
        parse("x+=1") => "",
        Assign::new(Variable::new("x"), lit_word("1"), BinOp::AddEq)
    );
    assert_eq!(
        parse("arr[0]='foo'") => "",
        Assign::new(
            Variable::with_subscripts("arr", vec![Subscript::new("0")]),
            Word::new(vec![SingleQuoted::new("foo").into()]),
            BinOp::Eq,
        )
    );
    assert_eq!(
        parse("foo=(bar baz)") => "",
        Assign::new(
            Variable::new("foo"),
            Array::new(vec![lit_word("bar").into(), lit_word("baz").into()]),
            BinOp::Eq,
        )
    );
    assert_eq!(
        parse("foo=( [bar]=(baz) )") => "",
        Assign::new(
            Variable::new("foo"),
            Array::new(vec![
                KeyValue::new(
                    vec![Subscript::new("bar")],
                    Array::new(vec![lit_word("baz").into()]),
                ).into()
            ]),
            BinOp::Eq,
        )
    );
    // Lint for double parens without spaces.
    assert_eq!(
        parse("foo=((bar))") => "",
        Assign::new(
            Variable::new("foo"),
            Array::new(vec![
                Array::new(vec![lit_word("bar").into()]).into()
            ]),
            BinOp::Eq,
        ),
        [((1, 5), (1, 7), NESTED_ARR)]
    );
    // Warn about empty values if they're not ending the command.
    assert_eq!(
        parse("foo=") => "",
        Assign::new(Variable::new("foo"), Value::Empty, BinOp::Eq)
    );
    assert_eq!(
        parse("foo= bar") => "bar",
        Assign::new(Variable::new("foo"), Value::Empty, BinOp::Eq),
        [((1, 5), (1, 6), SPACE_AFTER_EQ)]
    );
    // ...but IFS is the exception.
    assert_eq!(
        parse("IFS= ") => "",
        Assign::new(Variable::new("IFS"), Value::Empty, BinOp::Eq)
    );
    // Wrong assignment operator.
    assert_eq!(
        parse("foo==bar") => "",
        Assign::new(Variable::new("foo"), lit_word("=bar"), BinOp::Eq),
        [((1, 4), (1, 6), MISUSED_EQEQ)]
    );

    // Invalid identifier.
    assert_eq!(parse("1foo=bar") => Err((1, 1), Notes: [((1, 1), "Invalid identifier")]));
    // Cannot have a space before the operator.
    assert_eq!(parse("foo =bar") => Err((1, 4), Notes: [((1, 4), "Expected an assignment operator")]));
    // Variable names shouldn't be prefixed with a $.
    assert_eq!(parse("$foo=bar") => Err((1, 1), Notes: [((1, 1), "`$` on the left side")]));
}

#[test]
fn test_backquoted() {
    let parse = summarize(backquoted(false));

    // Lint for the wrong kind of backquote.
    assert_eq!(
        parse("´foo´") => "",
        BackQuoted::new(lit_pipeline("foo")),
        [((1, 1), (1, 2), FORWARD_TICKED), ((1, 5), (1, 6), FORWARD_TICKED)]
    );
    // Unclosed string warning.
    assert_eq!(
        parse("`foo\n`bar") => "bar",
        BackQuoted::new(lit_pipeline("foo")),
        [((1, 1), (2, 1), UNCLOSED_STRING), ((2, 1), SUS_CHAR_AFTER_QUOTE)]
    );
    // Make sure lints from the subparser are included.
    assert_eq!(
        parse("`foo\\nbar`") => "",
        BackQuoted::new(lit_pipeline("foonbar")),
        [((1, 5), (1, 7), UNESCAPED_WHITESPACE)]
    );
}

#[test]
fn test_backslash() {
    let parse = summarize(backslash);

    // An actual backslash.
    assert_eq!(parse("\\") => "", '\\');

    // A forward slash != backslash.
    assert_eq!(parse("/") => Err(1, 1));
}

#[test]
fn test_bats_test() {
    let parse = summarize(bats_test);

    // These tests are the ones that ShellCheck uses because idek what a bats test is.

    assert_eq!(
        parse("@test 'can parse' {\n  true\n}") => "",
        BatsTest::new("'can parse'", lit_pipeline("true"))
    );
    assert_eq!(
        parse("@test random text !(@*$Y&! {\n  true\n}") => "",
        BatsTest::new("random text !(@*$Y&!", lit_pipeline("true"))
    );
    assert_eq!(
        parse("@test foo { bar { baz {\n  true\n}") => "",
        BatsTest::new("foo { bar { baz", lit_pipeline("true"))
    );
    assert_eq!(parse("@test foo \n{\n true\n}") => Err(
        (1, 7),
        Notes: [((1, 7), "Invalid test name")]
    ));
}

#[test]
fn test_brace_expansion() {
    let parse = summarize(brace_expansion);

    // Valid sequence expressions.
    assert_eq!(
        parse("{1..5}") => "",
        BraceExpansion::new(vec![lit_word("1..5")]
    ));
    assert_eq!(
        parse("{$x..$y}") => "",
        BraceExpansion::new(vec![Word::new(vec![
            DollarExp::from(Variable::new("x")).into(),
            Lit::new("..").into(),
            DollarExp::from(Variable::new("y")).into(),
        ])])
    );
    // The closing brace can be escaped.
    assert_eq!(
        parse("{foo,\\}}") => "",
        BraceExpansion::new(vec![lit_word("foo"), lit_word("\\}")]
    ));
    // Nested expansions are legal.
    assert_eq!(
        parse("{foo.{txt,md}}") => "",
        BraceExpansion::new(vec![
            Word::new(vec![
                Lit::new("foo.").into(),
                BraceExpansion::new(vec![lit_word("txt"), lit_word("md")]).into()
            ])
        ],
    ));
    // Part of the expansion can be an empty word.
    assert_eq!(
        parse("{,foo}") => "",
        BraceExpansion::new(vec![Word::new(vec![]), lit_word("foo")]
    ));

    // Cannot be empty.
    assert_eq!(parse("{}") => Err((1, 2), Notes: [((1, 2), "Invalid sequence expression")]));
    // If there's a single element, it must be a valid sequence expression.
    assert_eq!(parse("{foo}") => Err((1, 2), Notes: [((1, 2), "Invalid sequence expression")]));
    assert_eq!(parse("{..5}") => Err((1, 2), Notes: [((1, 2), "Invalid sequence expression")]));
}

#[test]
fn test_brace_group() {
    let parse = summarize(brace_group);

    // Space after the opening brace is required except if we have {(.
    assert_eq!(
        parse("{foo;}") => "",
        Term::from(lit_pipeline("foo")),
        [((1, 2), MISSING_SPACE)]
    );
    // TODO: Add a test for {()}
    // The space before the `}` can be omitted if there's a semicolon.
    assert_eq!(parse("{ foo;}") => "", Term::from(lit_pipeline("foo")));

    // Empty code blocks aren't allowed.
    assert_eq!(parse("{ }") => Err(
        (1, 3),
        Lints: [((1, 1), (1, 4), EMPTY_BLOCK)],
        Notes: [((1, 3), "Unexpected token")]
    ));
    // The closing curly needs a semicolon or newline before it.
    assert_eq!(parse("{ foo}") => Err(
        (1, 7),
        Lints: [((1, 6), (1, 7), LITERAL_KEYWORD)],
        Notes: [((1, 7), "Missing right brace")]
    ));
}

#[test]
fn test_carriage_return() {
    let parse = summarize(carriage_return);

    // Check parsed CR and warning.
    assert_eq!(parse("\r") => "", '\r', [((1, 1), (1, 2), LITERAL_CR)]);

    // Not a CR.
    assert_eq!(parse("\n") => Err(1, 1));
}

#[test]
fn test_case_clause() {
    let parse = summarize(case_clause);

    // The opening parenthesis is optional.
    assert_eq!(
        parse("foo) bar;;") => "",
        CaseClause::new(
            vec![lit_word("foo")],
            Some(lit_pipeline("bar").into()),
            Some(ClauseSep::Break),
        )
    );

    // There can be space between the patterns.
    assert_eq!(
        parse("(  foo  |\tbar) baz;;&") => "",
        CaseClause::new(
            vec![lit_word("foo"), lit_word("bar")],
            Some(lit_pipeline("baz").into()),
            Some(ClauseSep::Continue),
        )
    );

    // The only invalid keyword is `esac`.
    assert_eq!(
        parse("if | do ) foo ;;") => "",
        CaseClause::new(
            vec![lit_word("if"), lit_word("do")],
            Some(lit_pipeline("foo").into()),
            Some(ClauseSep::Break),
        )
    );
    assert_eq!(
        parse("( esac ) foo ;&") => "",
        CaseClause::new(
            vec![lit_word("esac")],
            Some(lit_pipeline("foo").into()),
            Some(ClauseSep::Fallthrough),
        ),
        [((1, 3), (1, 7), LITERAL_KEYWORD)]
    );

    // The last clause can omit the separator.
    assert_eq!(
        parse("foo) bar\nesac") => "esac",
        CaseClause::new(
            vec![lit_word("foo")],
            Some(lit_pipeline("bar").into()),
            None,
        )
    );

    // Missing the separator between clauses.
    assert_eq!(parse("foo) bar\nbaz) qux") => Err(
        (2, 4),
        Notes: [((2, 4), "Did you forget the separator")]
    ));

    // Missing the closing parenthesis.
    assert_eq!(parse("foo | bar") => Err(
        (1, 10),
        Notes: [((1, 10), "Expected a closing `)`")]
    ));
}

#[test]
fn test_comment() {
    let parse = summarize(comment);

    // A Bash comment.
    assert_eq!(parse("# foo") => "", "# foo");
    // Make sure we stop at the end of a line.
    assert_eq!(parse("# foo\n") => "\n", "# foo");
    assert_eq!(parse("# foo\r\n") => "\r\n", "# foo");

    // Not a Bash comment.
    assert_eq!(parse("// foo") => Err(1, 1));
}

#[test]
fn test_cond() {
    let parse = summarize(cond);

    // Operators can be escaped or quoted.
    assert_eq!(
        parse("[ foo \\< bar ]") => "",
        Cond::with_expr(
            true,
            CondBinExpr::new(lit_word("foo"), lit_word("bar"), BinOp::Lt),
        )
    );
    assert_eq!(
        parse("[ foo '<' bar ]") => "",
        Cond::with_expr(
            true,
            CondBinExpr::new(lit_word("foo"), lit_word("bar"), BinOp::Lt),
        )
    );
    // Same with parentheses.
    assert_eq!(
        parse("[ '(' foo \\) ]") => "",
        Cond::with_expr(true, CondGroup::new(lit_word("foo")))
    );
    // ...In fact, you have to do it inside single brackets.
    assert_eq!(
        parse("[ '(' foo ) ]") => "",
        Cond::with_expr(true, CondGroup::new(lit_word("foo"))),
        [((1, 11), (1, 12), UNESCAPED_COND_GROUP)]
    );
    // ...But in double brackets is wrong to do it.
    assert_eq!(
        parse("[[ '(' foo ) ]]") => "",
        Cond::with_expr(false, CondGroup::new(lit_word("foo"))),
        [((1, 4), (1, 7), UNNECESSARY_COND_GROUP_ESCAPE)]
    );
    // New lines have to be escaped inside single brackets.
    assert_eq!(parse("[\n]") => "", Cond::new(true), [((1, 2), (2, 1), UNESCAPED_TEST_LF)]);
    // Empty conditions are legal.
    assert_eq!(parse("[[ ]]") => "", Cond::new(false));
    // Bracket mismatch.
    assert_eq!(
        parse("[ ]]") => "",
        Cond::new(true),
        [((1, 3), (1, 5), COND_BRACKET_MISMATCH)]
    );

    // if [ grep foo file ] pitfall.
    assert_eq!(parse("[ grep foo file ]") => Err(
        (1, 8),
        Lints: [((1, 3), (1, 7), BRACKETED_IF)],
        Notes: [((1, 8), "Expected the test to end")]
    ));
    // Incorrect opening bracket.
    assert_eq!(parse("]") => Err(1, 1));
    // Missing the closing bracket.
    assert_eq!(parse("[ ") => Err(
        (1, 3),
        Notes: [((1, 3), "Expected a non-empty word"), ((1, 3), "Invalid condition argument")]
    ));
    // Missing space after the opening bracket.
    assert_eq!(parse("[]") => Err(
        (1, 2),
        Lints: [((1, 2), MISSING_SPACE)],
        Notes: [((1, 2), "Unexpected bracket")]
    ));
    // Should use parens for grouping instead of [].
    assert_eq!(parse("[ [ foo ] ]") => Err(
        (1, 5),
        Lints: [((1, 3), (1, 4), TEST_GROUP)],
        Notes: [((1, 5), "Expected the test to end")]
    ));
    // Shouldn't use math operators inside test conditions.
    assert_eq!(parse("[ x + 1 ]") => Err(
        (1, 5),
        Lints: [((1, 5), (1, 6), MATH_IN_TEST)],
        Notes: [((1, 5), "Expected the test to end")]
    ));
    assert_eq!(parse("[ x - 1 ]") => Err(
        (1, 6),
        Lints: [((1, 5), (1, 6), MATH_IN_TEST)],
        Notes: [((1, 6), "Expected a test operator")]
    ));
    // Missing spaces around the operator.
    assert_eq!(parse("[ foo= bar ]") => Err(
        (1, 8),
        Lints: [((1, 6), MISSING_SPACE)],
        Notes: [((1, 8), "Expected the test to end")]
    ));
    assert_eq!(parse("[ foo\\>= bar ]") => Err(
        (1, 10),
        Lints: [((1, 7), MISSING_SPACE)],
        Notes: [((1, 10), "Expected the test to end")]
    ));
    // Word that prematurely ends the condition.
    assert_eq!(parse("[ foo] ]") => Err(
        (1, 7),
        Notes: [((1, 7), "Missing a space before this `]`")]
    ));
}

#[test]
fn test_dollar_cmd_expansion() {
    let parse = summarize(dollar_cmd_expansion);

    // The content can be any valid term.
    assert_eq!(parse("${ foo; }") => "", DollarCmdExpansion::new(lit_pipeline("foo")));

    // The term needs to end with a semicolon, else the closing curly will be
    // parsed as a literal.
    assert_eq!(parse("${ foo }") => Err(
        (1, 9),
        Lints: [((1, 8), (1, 9), LITERAL_KEYWORD)],
        Notes: [((1, 9), "Expected a `}`")]
    ));

    // There needs to be space after the `{`.
    assert_eq!(parse("${foo; }") => Err(1, 3));
}

#[test]
fn test_dollar_cmd_sub() {
    let parse = summarize(dollar_cmd_sub);

    // The content can be any valid term.
    assert_eq!(parse("$( foo )") => "", DollarCmdSub::new(Some(lit_pipeline("foo").into())));
    assert_eq!(
        parse("$(foo; ls 'bar')") => "",
        DollarCmdSub::new(Some(
            List::new(
                lit_pipeline("foo"),
                Pipeline::new(vec![
                    SimpleCmd::new(
                        Some(lit_word("ls")),
                        vec![],
                        vec![Word::new(vec![SingleQuoted::new("bar").into()]).into()],
                    ).into()
                ]),
                ControlOp::Semi
            ).into()
        ))
    );

    // The content can be just trivia.
    assert_eq!(parse("$( )") => "", DollarCmdSub::new(None));
    assert_eq!(parse("$(\n#foo\n)") => "", DollarCmdSub::new(None));

    // Missing parentheses.
    assert_eq!(parse("$)") => Err(1, 1));
    assert_eq!(parse("$(") => Err((1, 3), Notes: [((1, 3), "Expected a closing `)`")]));
}

#[test]
fn test_double_uniquote() {
    let parse = summarize(double_uniquote);

    // A legit uniquote.
    assert_eq!(parse("\u{201C}") => "", '\u{201C}', [((1, 1), (1, 2), UNICHAR)]);

    // Not a uniquote.
    assert_eq!(parse("\"") => Err(1, 1));
}

#[test]
fn test_identifier() {
    let parse = summarize(identifier);

    // Valid identifiers.
    assert_eq!(parse("var") => "", "var");
    assert_eq!(parse("_var0") => "", "_var0");

    // Identifiers can't start with a number.
    assert_eq!(parse("0var") => Err(1, 1));
    // Cannot be empty.
    assert_eq!(parse("") => Err(1, 1));
}

#[test]
fn test_if_cmd() {
    fn if_block(cond: &str, branch: &str) -> CondBlock {
        CondBlock::new(lit_pipeline(cond), lit_pipeline(branch))
    }

    let parse = summarize(if_cmd);

    // Vanilla if-else.
    assert_eq!(
        parse("if true; then\nfoo\nelse bar\nfi") => "",
        IfCmd::new(
            if_block("true", "foo"),
            vec![],
            Some(lit_pipeline("bar").into()),
        )
    );
    // Handle multiple `elif` branches and a missing `else` branch.
    assert_eq!(
        parse("if 0; then\nfoo\nelif 1; then\nbar\n elif 2\nthen baz\nfi") => "",
        IfCmd::new(
            if_block("0", "foo"),
            vec![if_block("1", "bar"), if_block("2", "baz")],
            None,
        )
    );
    // Warn about `then;`s.
    assert_eq!(
        parse("if true; then; foo; fi") => "",
        IfCmd::new(if_block("true", "foo"), vec![], None),
        [((1, 14), (1, 16), DISALLOWED_SEMI)]
    );
    // Warn when using `else if` in the same line.
    assert_eq!(
        parse("if 1; then foo; else if 2; then bar; fi fi") => "",
        IfCmd::new(
            if_block("1", "foo"),
            vec![],
            Some(Pipeline::new(
                vec![
                    CompoundCmd::new(
                        Construct::If(IfCmd::new(if_block("2", "bar"), vec![], None)),
                        vec![]
                    ).into()
                ]).into()
            )
        ),
        [((1, 17), (1, 25), ELIF_LIKE)]
    );
    // ...But don't if it's on a separate line.
    assert_eq!(
        parse("if 1; then foo; else\nif 2; then bar; fi fi") => "",
        IfCmd::new(
            if_block("1", "foo"),
            vec![],
            Some(Pipeline::new(
                vec![
                    CompoundCmd::new(
                        Construct::If(IfCmd::new(if_block("2", "bar"), vec![], None)),
                        vec![]
                    ).into()
                ]).into()
            )
        )
    );
    // Missing keywords.
    assert_eq!(parse("if true; foo; fi") => Err(
        (1, 15),
        Notes: [((1, 15), "Expected `then`")]
    ));
    assert_eq!(parse("if true; then foo") => Err(
        (1, 18),
        Notes: [((1, 18), "Expected `fi`")]
    ));
    // Empty clauses.
    assert_eq!(parse("if true; then\nfi") => Err(
        (2, 1),
        Notes: [((2, 1), "Empty `then`")]
    ));
    assert_eq!(parse("if true; then foo; else\nfi") => Err(
        (2, 1),
        Notes: [((2, 1), "Empty `else`")]
    ));
}

#[test]
fn test_in_list() {
    let parse = summarize(in_list);

    // Lists with the right terminator:
    assert_eq!(parse("in foo bar;") => "", vec![lit_word("foo"), lit_word("bar")]);
    assert_eq!(parse("in foo bar  \n  ") => "", vec![lit_word("foo"), lit_word("bar")]);

    // Inlined `do`s should emit a lint.
    assert_eq!(
        parse("in foo bar do baz") => "do baz",
        vec![lit_word("foo"), lit_word("bar")],
        [((1, 12), INLINED_DO)]
    );
}

#[test]
fn test_line_ending() {
    let parse = summarize(line_ending);

    // A new line is fine.
    assert_eq!(parse("\n") => "", '\n');
    // Parse CRLF but warn about the carriage return.
    assert_eq!(parse("\r\n") => "", '\n', [((1, 1), (1, 2), LITERAL_CR)]);

    // Missing the new line.
    assert_eq!(parse("\r") => Err((1, 2), Lints: [((1, 1), (1, 2), LITERAL_CR)]));
    // Not a new line at all.
    assert_eq!(parse("\t") => Err(1, 1));
}

#[test]
fn test_line_space() {
    let parse = summarize(line_space);

    // Spaces and tabs are fine.
    assert_eq!(parse(" ") => "", ' ');
    assert_eq!(parse("\t") => "", '\t');
    // Parse unicode spaces but emit warning.
    assert_eq!(parse("\u{A0}") => "", ' ', [((1, 1), (1, 2), UNICHAR)]);

    // Not a space.
    assert_eq!(parse("\n") => Err(1, 1));
}

#[test]
fn test_lit_sgmt() {
    let parse = summarize(|span| lit_sgmt("")(span));

    // Escaped literals that generate no warnings.
    assert_eq!(parse("\\$") => "", Lit::new("$"));
    assert_eq!(parse("\\{") => "", Lit::new("{"));
    // Warn about trailing space after a line continuation.
    assert_eq!(parse("\\  \n") => "", Lit::new(" "), [((1, 2), (2, 1), BS_TRAILING_SPACE)]);
    assert_eq!(parse("\\  ") => "", Lit::new(" "), [((1, 2), (1, 4), BS_TRAILING_SPACE)]);
    // Don't warn if it there's something between the backslash and the new line.
    assert_eq!(parse("\\  foo") => " foo", Lit::new(" "));
    // Line continuation + comment + line continuation are often mistakes.
    assert_eq!(parse("\\\n# foo\\\n") => "\n", Lit::new(""), [((2, 7), COMMENTED_BS_LF)]);
    // Don't warn if there's no comment.
    assert_eq!(parse("\\\nfoo") => "", Lit::new("foo"));
    // "Escaped characters" where the backslash is ignored.
    assert_eq!(parse("\\t") => "", Lit::new("t"), [((1, 1), (1, 3), UNESCAPED_WHITESPACE)]);
    assert_eq!(parse("\\a") => "", Lit::new("a"), [((1, 1), (1, 3), IGNORING_BS)]);
    // Some non-escaped, not special, sequence of characters.
    assert_eq!(parse("foo") => "", Lit::new("foo"));

    // Cannot be empty.
    assert_eq!(parse("") => Err(1, 1));
    // If not escaped, it cannot be a special character.
    assert_eq!(parse("$") => Err(1, 1));
}

#[test]
fn test_multi_trivia() {
    let parse = summarize(multi_trivia);

    // It's okay if the space/comment is on a single line.
    assert_eq!(parse(" ") => "", " ");
    assert_eq!(parse("# foo") => "", "# foo");
    // It's okay if some lines are empty.
    assert_eq!(parse("\n \n") => "", "\n \n");
    // Multiple comments.
    assert_eq!(parse("# foo \n #bar\n") => "", "# foo \n #bar\n");

    // Handle line continuations.
    assert_eq!(parse("\\\n# foo") => "", "# foo");
    // Stop when we should.
    assert_eq!(parse(" foo") => "foo", " ");
}

#[test]
fn test_pipeline() {
    let parse = summarize(pipeline);

    // A single command is a valid pipeline.
    assert_eq!(
        parse("! ls") => "",
        Pipeline::new(vec![lit_cmd("ls").into()])
    );
    // Make sure we can handle new lines between commands.
    assert_eq!(
        parse("ls |\n\ngrep .txt") => "",
        Pipeline::new(
            vec![
                lit_cmd("ls").into(),
                SimpleCmd::new(
                    Some(lit_word("grep")),
                    vec![],
                    vec![lit_word(".txt").into()],
                ).into()
            ]
        )
    );
    // Pipelines aren't separated with `||`.
    assert_eq!(parse("ls || grep .txt") => "|| grep .txt", lit_pipeline("ls"));

    // Error when the pipeline begins with a sus keyword.
    assert_eq!(parse("then") => Err((1, 1), Notes: [((1, 1), "Unexpected")]));
}

#[test]
fn test_redir() {
    let parse = summarize(redir);

    // The file descriptor can be omitted.
    assert_eq!(
        parse(">foo") => "",
        Redir::new(None, RedirOp::Great, lit_word("foo"))
    );
    // Make sure that operators that are prefixed by others are correctly parsed.
    assert_eq!(
        parse("<< foo") => "",
        Redir::new(None, RedirOp::DLess, lit_word("foo"))
    );
    // Valid file descriptor duplications:
    assert_eq!(
        parse("{foo}>&1-") => "",
        Redir::new(
            Some(Variable::new("foo").into()),
            RedirOp::GreatAnd,
            lit_word("1-"),
        )
    );
    assert_eq!(
        parse("{foo}>&{bar}") => "",
        Redir::new(
            Some(Variable::new("foo").into()),
            RedirOp::GreatAnd,
            lit_word("bar"),
        )
    );

    // Invalid file descriptor.
    assert_eq!(parse("{$foo}<<bar") => Err(
        (1, 1),
        Notes: [((1, 1), "Invalid file descriptor")]
    ));
    // Can't have a space before the operator.
    assert_eq!(parse("{foo} <<bar") => Err((1, 6), Notes: [((1, 6), "Expected a redirection operator")]));

    // TODO: Add a here doc test.
}

#[test]
fn test_simple_cmd() {
    let parse = summarize(simple_cmd);

    // Ignore backslashes for alias supressions.
    assert_eq!(parse("\\ls") => "", lit_cmd("ls"));
    // Can handle just a prefix.
    assert_eq!(
        parse("foo=bar") => "",
        SimpleCmd::new(
            None,
            vec![Assign::new(Variable::new("foo"), lit_word("bar"), BinOp::Eq).into()],
            vec![],
        )
    );
    // Prefixes can contain assignments or redirections.
    assert_eq!(
        parse("foo=bar >file ls") => "",
        SimpleCmd::new(
            Some(lit_word("ls")),
            vec![
                Assign::new(Variable::new("foo"), lit_word("bar"), BinOp::Eq).into(),
                Redir::new(None, RedirOp::Great, lit_word("file")).into(),
            ],
            vec![],
        )
    );
    // Warn when the command looks like a C-like comment.
    assert_eq!(
        parse("// echo") => "",
        SimpleCmd::new(
            Some(lit_word("//")),
            vec![],
            vec![lit_word("echo").into()]
        ),
        [((1, 1), (1, 3), C_LIKE_COMMENT)]
    );
    assert_eq!(
        parse("/* echo */") => "echo */",
        SimpleCmd::new(
            Some(Word::new(vec![Lit::new("/").into(), Glob::new("*").into()])),
            vec![],
            vec![]
        ),
        [((1, 1), (1, 3), C_LIKE_COMMENT)]
    );
    // Not using the right "else if" keyword.
    assert_eq!(
        parse("elseif") => "",
        lit_cmd("elseif"),
        [((1, 1), (1, 7), ELIF_LIKE)]
    );
    // // Let expressions (which can be quoted or not).
    // assert_eq!(
    //     parse("let x=0") => "",
    //     SimpleCmd::new(
    //         Some(lit_word("let")),
    //         vec![],
    //         vec![ArithSeq::new(vec![
    //             ArithBinExpr::new(
    //                 Variable::new("x"),
    //                 arith_number("0"),
    //                 BinOp::Eq
    //             ).into()
    //         ]).into()]
    //     )
    // );
    // assert_eq!(
    //     parse("let \"x = 0\"") => "",
    //     SimpleCmd::new(
    //         Some(lit_word("let")),
    //         vec![],
    //         vec![ArithSeq::new(vec![
    //             ArithBinExpr::new(
    //                 Variable::new("x"),
    //                 arith_number("0"),
    //                 BinOp::Eq
    //             ).into()
    //         ]).into()]
    //     )
    // );

    // Let expressions need arguments.
    assert_eq!(parse("let") => Err(
        (1, 4),
        Notes: [((1, 4), "Expected a non-empty word"), ((1, 4), "Expected an expression for this `let`")]
    ));
}

#[test]
fn test_single_quoted() {
    let parse = summarize(single_quoted);

    // A correctly written single quoted string.
    assert_eq!(parse("'hello world'") => "", SingleQuoted::new("hello world"));
    // An empty string is also fine.
    assert_eq!(parse("''") => "", SingleQuoted::new(""));
    // Warn when finding a uniquote.
    assert_eq!(parse("'let’s'") => "",
        SingleQuoted::new("let’s"),
        [((1, 5), (1, 6), UNICHAR)]
    );
    // Check if the user might be trying to escape a single quote.
    assert_eq!(
        parse("'escaped\\'") => "",
        SingleQuoted::new("escaped\\"),
        [((1, 10), UNESCAPED_SINGLE_QUOTE)]
    );
    // Might be an apostrophe, but it's not.
    assert_eq!(
        parse("'let's'") => "s'",
        SingleQuoted::new("let"),
        [((1, 5), (1, 6), APOSTROPHE)]
    );
    // Multi-line string, but the next character looks sus.
    assert_eq!(
        parse("'foo\n'bar") => "bar",
        SingleQuoted::new("foo\n"),
        [((1, 1), (2, 1), UNCLOSED_STRING), ((2, 1), SUS_CHAR_AFTER_QUOTE)]
    );

    // Make sure both quotes are present.
    assert_eq!(parse("foo'") => Err(1, 1));
    assert_eq!(parse("'foo") => Err((1, 5), Notes: [((1, 5), "Expected end of single quoted string")]));
}

#[test]
fn test_single_uniquote() {
    let parse = summarize(single_uniquote);

    // A legit uniquote.
    assert_eq!(parse("\u{2018}") => "", '\u{2018}', [((1, 1), (1, 2), UNICHAR)]);

    // Not a uniquote.
    assert_eq!(parse("'") => Err(1, 1));
}

#[test]
fn test_subshell() {
    let parse = summarize(subshell);

    // Since parentheses are operators, they don't need to be separated
    // from the list by whitespace.
    assert_eq!(parse("(ls)") => "", Term::from(lit_pipeline("ls")));
}

#[test]
fn test_term() {
    let parse = summarize(term);

    // Handle line continuations and lists of lists.
    assert_eq!(
        parse("ls;\\\necho 'foo' && echo 'bar'") => "",
        List::new(
            lit_pipeline("ls"),
            List::new(
                Pipeline::new(
                    vec![
                        SimpleCmd::new(
                            Some(lit_word("echo")),
                            vec![],
                            vec![Word::new(vec![SingleQuoted::new("foo").into()]).into()]
                        ).into()
                    ]
                ),
                Pipeline::new(
                    vec![
                        SimpleCmd::new(
                            Some(lit_word("echo")),
                            vec![],
                            vec![Word::new(vec![SingleQuoted::new("bar").into()]).into()]
                        ).into()
                    ]
                ),
                ControlOp::AndIf,
            ),
            ControlOp::Semi,
        )
    );
    // Warn when there's an HTML entity.
    assert_eq!(
        parse("foo &amp;&amp; bar") => ";&amp; bar",
        List::new(lit_pipeline("foo"), lit_pipeline("amp"), ControlOp::And),
        [((1, 5), (1, 10), HTML_ENTITY)]
    );
    // Check if something looks like URL query parameters.
    assert_eq!(
        parse("com&q=foo") => "",
        List::new(
            lit_pipeline("com"),
            Pipeline::new(
                vec![
                    SimpleCmd::new(
                        None,
                        vec![Assign::new(Variable::new("q"), lit_word("foo"), BinOp::Eq).into()],
                        vec![],
                    ).into()
                ]
            ),
            ControlOp::And,
        ),
        [((1, 4), (1, 5), UNSPACED_AMP)]
    );
}

#[test]
fn test_trivia() {
    let parse = summarize(trivia);

    // It's fine if there's nothing to parse.
    assert_eq!(parse("") => "", "");
    assert_eq!(parse("foo") => "foo", "");
    // Just space.
    assert_eq!(parse(" \t ") => "", " \t ");
    // Just a comment.
    assert_eq!(parse("# foo") => "", "# foo");
    // Allow comments to have line continuations if they're not preceded by one.
    assert_eq!(parse("# foo \\\n") => "\n", "# foo \\");
    // Space + line continuation + comment.
    assert_eq!(parse(" \\\n# foo") => "", " # foo");
    // Warn when the comment tries to have a line continuation.
    assert_eq!(
        parse(" \\\n# foo \\\n") => "\n",
        " # foo \\",
        [((2, 8), COMMENTED_BS_LF)]
    );
}

#[test]
fn test_trivia1() {
    let parse = summarize(trivia1);

    // Non-empty space.
    assert_eq!(parse(" ") => "", " ");

    // The parsed output cannot be empty.
    assert_eq!(parse("") => Err((1, 1), Notes: [((1, 1), "Expected whitespace")]));
    assert_eq!(parse("foo") => Err((1, 1), Notes: [((1, 1), "Expected whitespace")]));
}

// region: Shared constructors for common (and verbose) AST objects.
fn arith_number(n: &str) -> ArithExpansion {
    ArithExpansion::new(vec![Lit::new(n).into()])
}

fn lit_cmd(lit: &str) -> SimpleCmd {
    SimpleCmd::new(Some(lit_word(lit)), vec![], vec![])
}

fn lit_pipeline(lit: &str) -> Pipeline {
    Pipeline::new(vec![lit_cmd(lit).into()])
}

fn lit_word(lit: &str) -> Word {
    Word::new(vec![Lit::new(lit).into()])
}
// endregion

fn summarize<'a, P, R>(
    parser: P,
) -> impl Fn(
    &'a str,
) -> Result<
    (&'a str, R, Vec<(Range, &'static str)>),
    (
        Location,
        Vec<(Range, &'static str)>,
        Vec<(Location, &'static str)>,
    ),
>
where
    P: Fn(Span<'a>) -> ParseResult<R>,
{
    fn summarize_lints(lints: &[Lint]) -> Vec<(Range, &'static str)> {
        lints
            .into_iter()
            .map(|lint| (*lint.range(), lint.code()))
            .collect()
    }

    move |source| {
        parser(source_to_span(source))
            .finish()
            .map(|(span, res)| {
                (
                    *span.fragment(),
                    res,
                    summarize_lints(&span.extra.take_lints()),
                )
            })
            .map_err(|err| {
                let mut notes = err
                    .notes()
                    .into_iter()
                    .map(|note| (*note.location(), note.message()))
                    .collect::<Vec<_>>();

                // Sort by location and then alphabetically by the message.
                notes.sort_by(|(loc1, msg1), (loc2, msg2)| {
                    loc1.cmp(loc2).then_with(|| msg1.cmp(msg2))
                });

                (*err.location(), summarize_lints(err.lints()), notes)
            })
    }
}
