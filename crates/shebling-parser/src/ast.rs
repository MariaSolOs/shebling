use shebling_codegen::{FromVariants, New};

// TODO: Document each type.
// TODO: Simplify.

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
#[derive(Debug, New, PartialEq)]
pub(crate) struct BinExpr<O, L, R = L> {
    #[new(into)]
    left: Box<L>,
    #[new(into)]
    right: Box<R>,
    #[new(into)]
    op: O,
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
#[derive(Debug, New, PartialEq)]
pub(crate) struct UnExpr<O, E> {
    #[new(into)]
    expr: Box<E>,
    #[new(into)]
    op: O,
}
// endregion

// region: Arithmetic expressions.
#[derive(Debug, FromVariants, PartialEq)]
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
#[derive(Debug, New, PartialEq)]
pub(crate) struct ArithTriExpr {
    #[new(into)]
    cond: Box<ArithTerm>,
    #[new(into)]
    then_branch: Box<ArithTerm>,
    #[new(into)]
    else_branch: Box<ArithTerm>,
}
// endregion

// region: Shell expansions.
#[derive(Debug, FromVariants, PartialEq)]
pub(crate) enum DollarExp {
    Arith(ArithSeq),
    CmdExpansion(DollarCmdExpansion),
    CmdSub(DollarCmdSub),
    DoubleQuoting(DoubleQuoted),
    ParamExpansion(ParamExpansion),
    SingleQuoting(SingleQuoted),
    Var(SubscriptedVar),
}

#[derive(Debug, New, PartialEq)]
pub(crate) struct DollarCmdExpansion {
    #[new(into)]
    cmd: Term,
}

#[derive(Debug, New, PartialEq)]
pub(crate) struct DollarCmdSub {
    cmd: Option<Term>,
}

#[derive(Debug, New, PartialEq)]
pub struct ParamExpansion {
    sgmts: Vec<WordSgmt>,
}

#[derive(Debug, New, PartialEq)]
pub(crate) struct Glob {
    #[new(into)]
    pattern: String,
}

// TODO: Write a macro for these impls?
impl<S: AsRef<str> + ?Sized> PartialEq<S> for Glob {
    fn eq(&self, other: &S) -> bool {
        self.pattern == other.as_ref()
    }
}
// endregion

// region: Quoted strings.
#[derive(Debug, New, PartialEq)]
pub(crate) struct SingleQuoted {
    #[new(into)]
    string: String,
}

#[derive(Debug, New, PartialEq)]
pub(crate) struct DoubleQuoted {
    sgmts: Vec<DoubleQuotedSgmt>,
}

#[derive(Debug, FromVariants, PartialEq)]
pub(crate) enum DoubleQuotedSgmt {
    BackQuoted(BackQuoted),
    Lit(Lit),
    DollarExp(DollarExp),
}

#[derive(Debug, New, PartialEq)]
pub(crate) struct BackQuoted {
    #[new(into)]
    cmd: Term,
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

/// Sequence of [WordSgmt]s treated as a unit by the shell.
#[derive(Debug, New, PartialEq)]
pub(crate) struct Word {
    sgmts: Vec<WordSgmt>,
}

impl Word {
    pub(crate) fn sgmts(&self) -> &[WordSgmt] {
        &self.sgmts
    }

    /// If this [Word] represents a literal string, returns the [Lit].
    ///
    /// A word is considered a literal if:
    /// - The word has a single segment.
    /// - Such segment is a [WordSgmt::Lit].
    pub(crate) fn as_lit(&self) -> Option<&Lit> {
        match &self.sgmts.first() {
            Some(WordSgmt::Lit(lit)) if self.sgmts.len() == 1 => Some(lit),
            _ => None,
        }
    }
}

#[derive(Debug, FromVariants, PartialEq)]
pub(crate) enum WordSgmt {
    BackQuoted(BackQuoted),
    BraceExpansion(BraceExpansion),
    DollarExp(DollarExp),
    DoubleQuoted(DoubleQuoted),
    Glob(Glob),
    Lit(Lit),
    ProcSub(ProcSub),
    SingleQuoted(SingleQuoted),
}

#[derive(Debug, New, PartialEq)]
pub(crate) struct BraceExpansion {
    smgts: Vec<Word>,
}

#[derive(Debug, New, PartialEq)]
pub(crate) struct Lit {
    #[new(into)]
    value: String,
}

impl Lit {
    // TODO: Restrict the access to value?
    pub(crate) fn value(&self) -> &str {
        &self.value
    }
}

#[derive(Debug, New, PartialEq)]
pub(crate) struct ProcSub {
    list: Vec<Term>,
}

#[derive(Debug, New, PartialEq)]
pub(crate) struct SubscriptedVar {
    #[new(into)]
    ident: String,
    // Left unparsed for now.
    subscripts: Vec<String>,
}

/// A variable assignment (e.g. `foo='bar'`).
#[derive(Debug, New, PartialEq)]
pub(crate) struct Assign {
    var: SubscriptedVar,
    #[new(into)]
    value: Value,
    op: BinOp,
}

/// An assignment's value.
#[derive(Debug, FromVariants, PartialEq)]
pub(crate) enum Value {
    Array(Array),
    Empty,
    KeyValue(KeyValue),
    Word(Word),
}

/// An array variable.
#[derive(Debug, New, PartialEq)]
pub(crate) struct Array {
    elems: Vec<Value>,
}

/// [Array] element of the form `arr[subscript]=value`.
#[derive(Debug, New, PartialEq)]
pub(crate) struct KeyValue {
    key: Vec<String>,
    #[new(into)]
    value: Box<Value>,
}
// endregion

// region: Conditional expressions.
#[derive(Debug, New, PartialEq)]
pub(crate) struct CondOp {
    #[new(into)]
    op: String,
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
        self.op == other.as_ref()
    }
}

impl PartialEq<BinOp> for CondOp {
    fn eq(&self, other: &BinOp) -> bool {
        self.op == other.token()
    }
}

impl PartialEq<UnOp> for CondOp {
    fn eq(&self, other: &UnOp) -> bool {
        self.op == other.token()
    }
}

#[derive(Debug, FromVariants, PartialEq)]
pub(crate) enum CondExpr {
    BinExpr(CondBinExpr),
    Group(CondGroup),
    UnExpr(CondUnExpr),
    Word(Word),
}

pub(crate) type CondBinExpr = BinExpr<CondOp, CondExpr>;

pub(crate) type CondUnExpr = UnExpr<CondOp, CondExpr>;

#[derive(Debug, New, PartialEq)]
pub(crate) struct CondGroup {
    #[new(into)]
    group: Box<CondExpr>,
}

/// Conditional expression enclosed in `[..]` or `[[..]]`.
#[derive(Debug, New, PartialEq)]
pub(crate) struct Cond {
    single_bracketed: bool,
    expr: Option<CondExpr>,
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
#[derive(Debug, FromVariants, PartialEq)]
pub(crate) enum FileDesc {
    Number,
    StdOutErr,
    Var(SubscriptedVar),
}

/// Shell redirection, used to change the files a [Cmd] reads and writes to.
#[derive(Debug, New, PartialEq)]
pub(crate) struct Redir {
    file_desc: Option<FileDesc>,
    op: RedirOp,
    word: Word,
}
// endregion

// region: Commands.
/// A sequence of words defining a shell command.
#[derive(Debug, FromVariants, PartialEq)]
pub(crate) enum Cmd {
    Compound(CompoundCmd),
    Cond(CondCmd),
    Coproc(Coproc),
    Simple(SimpleCmd),
}

/// [Cmd] defined by a particular [Keyword] followed by a shell programming
/// language construct.
#[derive(Debug, New, PartialEq)]
pub(crate) struct CompoundCmd {
    cmd: Construct,
    redirs: Vec<Redir>,
}

#[derive(Debug, New, PartialEq)]
pub(crate) struct CondCmd {
    cond: Cond,
    redirs: Vec<Redir>,
}

#[derive(Debug, New, PartialEq)]
pub(crate) struct Coproc {
    name: Option<String>,
    #[new(into)]
    cmd: Box<Cmd>,
}

/// Sequence of [Word]s separated by blanks, terminated by a [ControlOp].
#[derive(Debug, New, PartialEq)]
pub(crate) struct SimpleCmd {
    cmd: Option<Word>,
    prefix: Vec<CmdPrefixSgmt>,
    suffix: Vec<CmdSuffixSgmt>,
}

/// A [Word] that precedes the command in a [SimpleCmd].
#[derive(Debug, FromVariants, PartialEq)]
pub(crate) enum CmdPrefixSgmt {
    Assign(Assign),
    Redir(Redir),
}

/// A [Word] that follows the command in a [SimpleCmd].
#[derive(Debug, FromVariants, PartialEq)]
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

/// Either a "leaf" [Pipeline], or a [BinExpr] of [Term]s.
#[derive(Debug, FromVariants, PartialEq)]
pub(crate) enum Term {
    List(List),
    Pipeline(Pipeline),
}

/// A sequence of [Cmd]s separated by [ControlOp]s.
#[derive(Debug, New, PartialEq)]
pub(crate) struct Pipeline {
    cmds: Vec<Cmd>,
}

/// A sequence of [Term]s separated by [ControlOp]s.
pub(crate) type List = BinExpr<ControlOp, Term>;
// endregion

// region: Command constructs.
#[derive(Debug, PartialEq)]
pub(crate) enum Construct {
    BatsTest(Function),
    BraceGroup(Term),
    Case(CaseCmd),
    ForLoop(ForLoop),
    Function(Function),
    If(IfCmd),
    Select(InListed),
    Subshell(Term),
    Until(CondBlock),
    While(CondBlock),
}

/// Conditional command construct beginning with [Keyword::Case].
#[derive(Debug, New, PartialEq)]
pub(crate) struct CaseCmd {
    word: Word,
    clauses: Vec<CaseClause>,
}

/// Clause in a [CaseCmd].
#[derive(Debug, New, PartialEq)]
pub(crate) struct CaseClause {
    pattern: Vec<Word>,
    cmd: Option<Term>,
    sep: Option<ClauseSep>,
}

tokenizable! {
    /// Terminators of a [CaseClause].
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

/// Looping command construct beginning with [Keyword::For].
#[derive(Debug, FromVariants, PartialEq)]
pub(crate) enum ForLoop {
    Arith(ArithForLoop),
    Listed(InListed),
}

/// Arithmetic, `C`-style [ForLoop].
#[derive(Debug, New, PartialEq)]
pub(crate) struct ArithForLoop {
    header: (ArithSeq, ArithSeq, ArithSeq),
    #[new(into)]
    body: Term,
}

/// Group of commands with a `name in lists` header.
#[derive(Debug, New, PartialEq)]
pub(crate) struct InListed {
    #[new(into)]
    name: String,
    list: Vec<Word>,
    #[new(into)]
    body: Term,
}

/// Group of commands that can be later executed by its name.
#[derive(Debug, New, PartialEq)]
pub(crate) struct Function {
    #[new(into)]
    name: String,
    #[new(into)]
    body: Term,
}

/// Conditional command construct beginning with [Keyword::If].
#[derive(Debug, New, PartialEq)]
pub(crate) struct IfCmd {
    if_branch: CondBlock,
    elif_branches: Vec<CondBlock>,
    else_term: Option<Term>,
}

/// Conditional block of commands that is executed when its condition is met.
#[derive(Debug, New, PartialEq)]
pub(crate) struct CondBlock {
    #[new(into)]
    cond: Term,
    #[new(into)]
    block: Term,
}
// endregion
