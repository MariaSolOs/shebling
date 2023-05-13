use derive_more::From;

// region: Tokens.
/// Trait for types that represent single tokens, such as keywords
/// and operators.
pub(crate) trait Token
where
    Self: Copy + Sized,
{
    fn token(&self) -> &'static str;
}

/// Utility macro for creating [Token] enums.
macro_rules! tokenizable {
    (
        $(#[doc = $enum_doc:expr])*
        enum $name:ident {
            $($(#[doc = $arm_doc:expr])* $arm:ident($token:literal),)+
        }
     ) => {
        $(#[doc = $enum_doc])*
        #[derive(Clone, Copy, Debug, PartialEq)]
        pub(crate) enum $name {
            $(
                /// ```sh
                #[doc = $token]
                /// ```
                ///
                $(#[doc = $arm_doc])*
                $arm
            ),+
        }

        impl self::Token for $name {
            fn token(&self) -> &'static str {
                match self {
                    $(Self::$arm => $token),+
                }
            }
        }
    };
}
// endregion

// region: Binary and unary expressions.
tokenizable! {
    /// A binary operator.
    enum BinOp {
        /// Addition operator.
        Add("+"),

        /// Addition assignment operator.
        AddEq("+="),

        /// Logical-and operator.
        And("&&"),

        /// Bitwise-and operator.
        BitAnd("&"),

        /// Bitwise-and assignment operator.
        BitAndEq("&="),

        /// Bitwise-or operator.
        BitOr("|"),

        /// Bitwise-or assignment operator.
        BitOrEq("|="),

        /// Bitwise-xor operator.
        BitXor("^"),

        /// Bitwise-xor assignment operator.
        BitXorEq("^="),

        /// Division operator.
        Div("/"),

        /// Division assignment operator.
        DivEq("/="),

        /// Assignment operator. In conditional expressions, it is also
        /// used for string equality.
        Eq("="),

        /// Equality operator. In conditional expressions, it is also
        /// used for string equality and pattern matching.
        EqEq("=="),

        /// Greater-than-or-equal-to operator.
        Ge(">="),

        /// Greater-than operator.
        Gt(">"),

        /// Less-than-or-equal-to operator.
        Le("<="),

        /// Less-than operator.
        Lt("<"),

        /// Regex match operator.
        Match("=~"),

        /// Modulus operator.
        Mod("%"),

        /// Modulus assignment operator.
        ModEq("%="),

        /// Multiplication operator.
        Mult("*"),

        /// Multiplication assignment operator.
        MultEq("*="),

        /// Not-equal-to operator.
        Ne("!="),

        /// Logical-or operator.
        Or("||"),

        /// Exponentiation operator.
        Pow("**"),

        /// Left-shift operator.
        Shl("<<"),

        /// Left-shift assignment operator.
        ShlEq("<<="),

        /// Right-shift operator.
        Shr(">>"),

        /// Right-shift assignment operator.
        ShrEq(">>="),

        /// Subtraction operator.
        Sub("-"),

        /// Subtraction assignment operator.
        SubEq("-="),
    }
}

/// A binary expression (e.g. `$x + 1`).
#[derive(Debug, PartialEq)]
pub(crate) struct BinExpr<O, L, R = L> {
    left: Box<L>,
    right: Box<R>,
    op: O,
}

impl<O, L, R> BinExpr<O, L, R> {
    /// Creates a new binary expression.
    pub(crate) fn new(left: impl Into<L>, right: impl Into<R>, op: impl Into<O>) -> Self {
        Self {
            left: Box::new(left.into()),
            right: Box::new(right.into()),
            op: op.into(),
        }
    }
}

tokenizable! {
    /// A unary operator.
    enum UnOp {
        /// Bitwise negation operator.
        BitNeg("~"),

        /// Pre/post decrement operator.
        Dec("--"),

        /// Pre/post increment operator.
        Inc("++"),

        /// Unary minus operator.
        Neg("-"),

        /// Logical negation operator.
        Not("!"),

        /// Unary plus operator.
        Pos("+"),
    }
}

/// A unary expression (e.g. `++x`).
#[derive(Debug, PartialEq)]
pub(crate) struct UnExpr<O, E> {
    expr: Box<E>,
    op: O,
}

impl<O, E> UnExpr<O, E> {
    /// Creates a new unary expression.
    pub(crate) fn new(expr: impl Into<E>, op: impl Into<O>) -> Self {
        Self {
            expr: Box::new(expr.into()),
            op: op.into(),
        }
    }
}
// endregion

// region: Arithmetic expressions.
#[derive(Debug, From, PartialEq)]
pub(crate) enum ArithTerm {
    /// Binary expression (e.g. `$x + 1`).
    BinExpr(BinExpr<BinOp, ArithTerm>),

    /// Shell expansion.
    Expansion(Vec<WordSgmt>),

    /// Parenthesized expression.
    Group(ArithSeq),

    /// Conditional expression with a ternary operator.
    TriExpr(ArithTriExpr),

    /// Unary expression (e.g. `++x`).
    UnExpr(UnExpr<UnOp, ArithTerm>),

    /// A (maybe indexed) variable.
    Var(SubscriptedVar),
}

/// Comma-delimited sequence of arithmetic terms.
pub(crate) type ArithSeq = Vec<ArithTerm>;

/// An arithmetic ternary expression of the form
/// `condition ? true_branch : else_branch`.
#[derive(Debug, PartialEq)]
pub(crate) struct ArithTriExpr {
    cond: Box<ArithTerm>,
    then_branch: Box<ArithTerm>,
    else_branch: Box<ArithTerm>,
}

impl ArithTriExpr {
    /// Creates a new arithmetic ternary expression.
    pub(crate) fn new(
        cond: impl Into<ArithTerm>,
        then_branch: impl Into<ArithTerm>,
        else_branch: impl Into<ArithTerm>,
    ) -> Self {
        Self {
            cond: Box::new(cond.into()),
            then_branch: Box::new(then_branch.into()),
            else_branch: Box::new(else_branch.into()),
        }
    }
}
// endregion

// region: Shell expansions.
#[derive(Debug, From, PartialEq)]
pub(crate) enum DollarExp {
    /// Arithmetic expansion. Can happen inside `$(())` or `$[]`.
    Arith(ArithSeq),

    /// `ksh`-style command expansion (e.g. `${ foo; }`).
    CmdExpansion(Term),

    /// Dollar-prefixed command substitution (e.g. `$(foo)`).
    ///
    /// The contained [Term] will be [None] iff the command substitution is `$()`.
    CmdSub(Option<Term>),

    /// `$""` expression, used for locale-specific translation.
    DoubleQuoting(DoubleQuoted),

    /// Shell parameter expansion (e.g. `${!foo[@]}`).
    ParamExpansion(Vec<WordSgmt>),

    /// `$''` expression, used for ANSI-C quoting.
    #[from(ignore)]
    SingleQuoting(String),

    /// A variable.
    #[from(ignore)]
    Var(String),
}
// endregion

// region: Words.
tokenizable! {
    /// Reserved words that have special meaning to the shell. They are used to
    /// begin and end the shell's compound commands.
    ///
    /// Note that [bash](https://www.gnu.org/software/bash/manual/bash.html#Reserved-Words)
    /// also considers `!`, `[[`, `]]`, `{` and `}` to be keywords, but because
    /// their parsing depends on where they appear, we don't include them here.
    enum Keyword {
        Case("case"),
        Coproc("coproc"),
        Do("do"),
        Done("done"),
        Elif("elif"),
        Else("else"),
        Esac("esac"),
        Fi("fi"),
        For("for"),
        Function("function"),
        If("if"),
        In("in"),
        Select("select"),
        Then("then"),
        Until("until"),
        While("while"),
    }
}

/// Sequence of [word segments](WordSgmt) treated as a unit by the shell.
#[derive(Debug, PartialEq)]
pub(crate) struct Word(Vec<WordSgmt>);

impl Word {
    /// Creates a new [Word] containing the given [segments](WordSgmt).
    pub(crate) fn new(sgmts: Vec<WordSgmt>) -> Self {
        Self(sgmts)
    }

    /// Returns a reference to this [Word]'s segments.
    pub(crate) fn sgmts(&self) -> &[WordSgmt] {
        &self.0
    }

    /// If this [Word] represents a literal string, returns the literal.
    ///
    /// A word is considered a literal if:
    /// - The word has a single segment.
    /// - Such segment is a [WordSgmt::Lit].
    pub(crate) fn as_lit(&self) -> Option<&str> {
        match &self.0.first() {
            Some(WordSgmt::Lit(lit)) if self.0.len() == 1 => Some(lit),
            _ => None,
        }
    }
}

#[derive(Debug, From, PartialEq)]
pub(crate) enum WordSgmt {
    /// Backquoted command substitution.
    BackQuoted(Term),

    /// Brace expansion.
    BraceExpansion(Vec<Word>),

    /// Dollar-prefixed expression.
    #[from]
    DollarExp(DollarExp),

    /// Double-quoted string.
    #[from]
    DoubleQuoted(DoubleQuoted),

    /// Pattern match sequence.
    Glob(String),

    /// Literal, unquoted string.
    Lit(String),

    /// Process substitution.
    ProcSub(Vec<Term>),

    /// Single-quoted string.
    SingleQuoted(String),
}

/// A sequence of [word segments][DoubleQuotedSgmt] enclosed in `""`.
#[derive(Debug, PartialEq)]
pub(crate) struct DoubleQuoted(Vec<DoubleQuotedSgmt>);

impl DoubleQuoted {
    /// Creates a new double-quoted string.
    pub(crate) fn new(sgmts: Vec<DoubleQuotedSgmt>) -> Self {
        Self(sgmts)
    }
}

#[derive(Debug, From, PartialEq)]
pub(crate) enum DoubleQuotedSgmt {
    BackQuoted(Term),
    Lit(String),
    DollarExp(DollarExp),
}

/// A subscripted variable, used for indexing into [Array]s.
#[derive(Debug, PartialEq)]
pub(crate) struct SubscriptedVar {
    ident: String,
    // Left unparsed for now.
    subscripts: Vec<String>,
}

impl SubscriptedVar {
    /// Creates a new subscripted variable with the given identifier
    /// and subscripts.
    pub(crate) fn new(ident: impl Into<String>, subscripts: Vec<String>) -> Self {
        Self {
            ident: ident.into(),
            subscripts,
        }
    }
}

/// A variable assignment (e.g. `foo='bar'`).
#[derive(Debug, PartialEq)]
pub(crate) struct Assign {
    var: SubscriptedVar,
    value: Value,
    op: BinOp,
}

impl Assign {
    /// Creates a new variable assignment, where `value` is assigned
    /// to `var`.
    ///
    /// The assignment can be a compound assignment when `op` is different
    /// from [=](BinOp::Eq) (e.g. `x+=1`).
    pub(crate) fn new(var: SubscriptedVar, value: impl Into<Value>, op: BinOp) -> Self {
        Self {
            var,
            value: value.into(),
            op,
        }
    }
}

/// An assignment's value.
#[derive(Debug, From, PartialEq)]
pub(crate) enum Value {
    Array(Array),
    Empty,
    KeyValue(KeyValue),
    Word(Word),
}

/// An array variable.
#[derive(Debug, PartialEq)]
pub(crate) struct Array(Vec<Value>);

impl Array {
    /// Creates a new [array](Array) containing the given [values](Value).
    pub(crate) fn new(values: Vec<Value>) -> Self {
        Self(values)
    }
}

/// [Array] element of the form `arr[subscript]=value`.
#[derive(Debug, PartialEq)]
pub(crate) struct KeyValue {
    key: Vec<String>,
    value: Box<Value>,
}

impl KeyValue {
    /// Creates a new array [key-value element](KeyValue).
    pub(crate) fn new(key: Vec<String>, value: impl Into<Value>) -> Self {
        Self {
            key,
            value: Box::new(value.into()),
        }
    }
}
// endregion

// region: Conditional expressions.
#[derive(Debug, PartialEq)]
pub(crate) struct CondOp(String);

impl CondOp {
    /// Creates a new [conditional operator](CondOp).
    pub(crate) fn new(op: impl Into<String>) -> Self {
        Self(op.into())
    }
}

impl From<BinOp> for CondOp {
    fn from(value: BinOp) -> Self {
        Self::new(value.token())
    }
}

impl From<UnOp> for CondOp {
    fn from(value: UnOp) -> Self {
        Self::new(value.token())
    }
}

impl<S: AsRef<str> + ?Sized> PartialEq<S> for CondOp {
    fn eq(&self, other: &S) -> bool {
        self.0 == other.as_ref()
    }
}

impl PartialEq<BinOp> for CondOp {
    fn eq(&self, other: &BinOp) -> bool {
        self.0 == other.token()
    }
}

impl PartialEq<UnOp> for CondOp {
    fn eq(&self, other: &UnOp) -> bool {
        self.0 == other.token()
    }
}

#[derive(Debug, From, PartialEq)]
pub(crate) enum CondExpr {
    /// A binary expression (e.g. `x -eq y`).
    BinExpr(BinExpr<CondOp, CondExpr>),

    /// Parenthesized expression (e.g. `( x -eq y )`).
    Group(Box<CondExpr>),

    /// A binary expression (e.g. `-z x`).
    UnExpr(UnExpr<CondOp, CondExpr>),

    /// A nullary expression.
    Word(Word),
}

/// Conditional expression enclosed in `[..]` or `[[..]]`.
#[derive(Debug, PartialEq)]
pub(crate) struct Cond {
    single_bracketed: bool,
    expr: Option<CondExpr>,
}

impl Cond {
    /// Creates a new conditional expression.
    pub(crate) fn new(single_bracketed: bool, expr: Option<CondExpr>) -> Self {
        Self {
            single_bracketed,
            expr,
        }
    }
}
// endregion

// region: Redirections.
tokenizable! {
    /// [Token] that performs a redirection function.
    enum RedirOp {
        Clobber(">|"),
        DGreat(">>"),
        DLess("<<"),
        Great(">"),
        GreatAnd(">&"),
        Less("<"),
        LessAnd("<&"),
        LessGreat("<>"),
        TLess("<<<"),
    }
}

/// File descriptor that describes the file handle manipulated
/// by a redirection.
#[derive(Debug, From, PartialEq)]
pub(crate) enum FileDesc {
    Number,
    StdOutErr,
    Var(String),
}

/// Shell redirection, used to change the files a [command](Cmd) reads
/// and writes to.
#[derive(Debug, PartialEq)]
pub(crate) struct Redir {
    file_desc: Option<FileDesc>,
    op: RedirOp,
    word: Word,
}

impl Redir {
    /// Creates a new redirection.
    pub(crate) fn new(file_desc: Option<FileDesc>, op: RedirOp, word: Word) -> Self {
        Self {
            file_desc,
            op,
            word,
        }
    }
}
// endregion

// region: Commands.
/// A sequence of words defining a shell command.
#[derive(Debug, From, PartialEq)]
pub(crate) enum Cmd {
    Compound(CompoundCmd),
    Coproc(Coproc),
    Simple(SimpleCmd),
}

/// [Command](Cmd) beginning with a particular [Keyword] and formed by
/// a shell programming language construct.
#[derive(Debug, PartialEq)]
pub(crate) struct CompoundCmd {
    cmd: Construct,
    redirs: Vec<Redir>,
}

impl CompoundCmd {
    /// Creates a new compound command.
    pub(crate) fn new(cmd: impl Into<Construct>, redirs: Vec<Redir>) -> Self {
        Self {
            cmd: cmd.into(),
            redirs,
        }
    }
}

/// [Command](Cmd) preceded by [`coproc`](Keyword::Coproc). A [coprocess](Coproc)
/// is executed asynchronously in a subshell.
#[derive(Debug, PartialEq)]
pub(crate) struct Coproc {
    name: Option<String>,
    cmd: Box<Cmd>,
}

impl Coproc {
    /// Creates a new coprocess.
    pub(crate) fn new(name: Option<String>, cmd: impl Into<Cmd>) -> Self {
        Self {
            name,
            cmd: Box::new(cmd.into()),
        }
    }
}

/// Sequence of [Word]s separated by blanks, terminated by a
/// [control operator](ControlOp).
#[derive(Debug, PartialEq)]
pub(crate) struct SimpleCmd {
    cmd: Option<Word>,
    prefix: Vec<CmdPrefixSgmt>,
    suffix: Vec<CmdSuffixSgmt>,
}

impl SimpleCmd {
    /// Creates a new simple command.
    pub(crate) fn new(
        cmd: Option<Word>,
        prefix: Vec<CmdPrefixSgmt>,
        suffix: Vec<CmdSuffixSgmt>,
    ) -> Self {
        Self {
            cmd,
            prefix,
            suffix,
        }
    }
}

/// A [Word] that precedes the command in a [simple command](SimpleCmd).
#[derive(Debug, From, PartialEq)]
pub(crate) enum CmdPrefixSgmt {
    Assign(Assign),
    Redir(Redir),
}

/// A [Word] that follows the command in a [simple command](SimpleCmd).
#[derive(Debug, From, PartialEq)]
pub(crate) enum CmdSuffixSgmt {
    ArithSeq(ArithSeq),
    Assign(Assign),
    Pipeline(Pipeline),
    Redir(Redir),
    Word(Word),
}

tokenizable! {
    /// [Token] that performs a control function.
    enum ControlOp {
        And("&"),
        AndIf("&&"),
        DSemi(";;"),
        Newline("\n"),
        Or("|"),
        OrAnd("|&"),
        OrIf("||"),
        Semi(";"),
    }
}

/// Either a "leaf" [Pipeline], or a [binary expression](BinExpr) of [Term]s.
#[derive(Debug, From, PartialEq)]
pub(crate) enum Term {
    List(List),
    Pipeline(Pipeline),
}

/// A sequence of [commands](Cmd) separated by [|](ControlOp::Or) or
/// [|&](ControlOp::OrAnd).
#[derive(Debug, PartialEq)]
pub(crate) struct Pipeline(Vec<Cmd>);

impl Pipeline {
    /// Creates a new [Pipeline] containing the given [commands](Cmd).
    pub(crate) fn new(cmds: Vec<Cmd>) -> Self {
        Self(cmds)
    }
}

/// A sequence of [Term]s separated by [control operators](ControlOp).
pub(crate) type List = BinExpr<ControlOp, Term>;
// endregion

// region: Command constructs.
/// A shell programming language construct, which form the body of a
/// [compound command](CompoundCmd).
#[derive(Debug, From, PartialEq)]
pub(crate) enum Construct {
    BatsTest(Function),
    BraceGroup(Term),
    #[from]
    Case(CaseCmd),
    #[from]
    Cond(Cond),
    #[from]
    ForLoop(ForLoop),
    Function(Function),
    #[from]
    If(IfCmd),
    Select(InListed),
    Subshell(Term),
    Until(CondBlock),
    While(CondBlock),
}

/// Conditional command construct beginning with [`case`](Keyword::Case).
#[derive(Debug, PartialEq)]
pub(crate) struct CaseCmd {
    word: Word,
    clauses: Vec<CaseClause>,
}

impl CaseCmd {
    /// Creates a new case command.
    pub(crate) fn new(word: Word, clauses: Vec<CaseClause>) -> Self {
        Self { word, clauses }
    }
}

/// Clause in a [case command](CaseCmd).
#[derive(Debug, PartialEq)]
pub(crate) struct CaseClause {
    pattern: Vec<Word>,
    cmd: Option<Term>,
    sep: Option<ClauseSep>,
}

impl CaseClause {
    /// Creates a new [case clause](CaseClause).
    pub(crate) fn new(pattern: Vec<Word>, cmd: Option<Term>, sep: Option<ClauseSep>) -> Self {
        Self { pattern, cmd, sep }
    }
}

tokenizable! {
    /// Terminators of a [case clause](CaseClause).
    enum ClauseSep {
        /// No subsequent matches are attempted after the first pattern match.
        Break(";;"),

        /// Test the patterns in the next clause, if any, and execute any
        /// associated command-list on a successful match.
        Continue(";;&"),

        /// Execute the command-list associated with the next clause, if any.
        Fallthrough(";&"),
    }
}

/// Looping command construct beginning with ["for"](Keyword::For).
#[derive(Debug, From, PartialEq)]
pub(crate) enum ForLoop {
    Arith(ArithForLoop),
    Listed(InListed),
}

/// Arithmetic, `C`-style [for-loop](ForLoop).
#[derive(Debug, PartialEq)]
pub(crate) struct ArithForLoop {
    header: (ArithSeq, ArithSeq, ArithSeq),
    body: Term,
}

impl ArithForLoop {
    /// Creates a new arithmetic for-loop.
    pub(crate) fn new(header: (ArithSeq, ArithSeq, ArithSeq), body: impl Into<Term>) -> Self {
        Self {
            header,
            body: body.into(),
        }
    }
}

/// Group of commands with a `name in lists` header.
#[derive(Debug, PartialEq)]
pub(crate) struct InListed {
    name: String,
    list: Vec<Word>,
    body: Term,
}

impl InListed {
    /// Creates a new group of commands to be executed in an `in lists` loop.
    pub(crate) fn new(name: impl Into<String>, list: Vec<Word>, body: impl Into<Term>) -> Self {
        Self {
            name: name.into(),
            list,
            body: body.into(),
        }
    }
}

/// Group of commands that can be later executed by its name.
#[derive(Debug, PartialEq)]
pub(crate) struct Function {
    name: String,
    body: Term,
}

impl Function {
    /// Creates a new function.
    pub(crate) fn new(name: impl Into<String>, body: impl Into<Term>) -> Self {
        Self {
            name: name.into(),
            body: body.into(),
        }
    }
}

/// Conditional command construct beginning with [`if`](Keyword::If).
#[derive(Debug, PartialEq)]
pub(crate) struct IfCmd {
    if_branch: CondBlock,
    elif_branches: Vec<CondBlock>,
    else_term: Option<Term>,
}

impl IfCmd {
    /// Creates a new `if` command.
    pub(crate) fn new(
        if_branch: CondBlock,
        elif_branches: Vec<CondBlock>,
        else_term: Option<Term>,
    ) -> Self {
        Self {
            if_branch,
            elif_branches,
            else_term,
        }
    }
}

/// Conditional block of commands that is executed when its condition is met.
#[derive(Debug, PartialEq)]
pub(crate) struct CondBlock {
    cond: Term,
    block: Term,
}

impl CondBlock {
    /// Creates a new conditional block.
    pub(crate) fn new(cond: impl Into<Term>, block: impl Into<Term>) -> Self {
        Self {
            cond: cond.into(),
            block: block.into(),
        }
    }
}
// endregion
