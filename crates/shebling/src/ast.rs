// TODO: Remove this.
#![allow(dead_code)]

use crate::parser::Span;
use shebling_codegen::{from_structs, New};

// TODO: Define a Trivia type.

// region: Tokens.
/// Trait for types that represent single tokens, such as keywords
/// and operators.
pub(crate) trait Token
where
    Self: Copy + Sized,
{
    fn token(&self) -> &'static str;
}

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

// region: Symbol location.
#[derive(Clone, Copy, Debug, Eq, New, Ord, PartialEq, PartialOrd)]
pub(crate) struct Location {
    line: u32,
    column: usize,
}

impl Location {
    pub(crate) fn line(&self) -> &u32 {
        &self.line
    }

    pub(crate) fn column(&self) -> &usize {
        &self.column
    }
}

impl From<Span<'_>> for Location {
    fn from(value: Span<'_>) -> Self {
        Self::from(&value)
    }
}

impl From<&Span<'_>> for Location {
    fn from(value: &Span) -> Self {
        Self::new(value.location_line(), value.get_utf8_column())
    }
}

#[derive(Clone, Copy, Debug, New, PartialEq)]
pub(crate) struct Range {
    #[new(into)]
    start: Location,
    #[new(into)]
    end: Location,
}

impl<L: Into<Location>> From<L> for Range {
    fn from(value: L) -> Self {
        let location = value.into();
        Self::new(location, location)
    }
}
// endregion

// region: Binary and unary expressions.
tokenizable! {
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

tokenizable! {
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

#[derive(Debug, New, PartialEq)]
pub(crate) struct BinExpr<O, L, R = L> {
    #[new(into)]
    left: Box<L>,
    #[new(into)]
    right: Box<R>,
    #[new(into)]
    op: O,
}

#[derive(Debug, New, PartialEq)]
pub(crate) struct UnExpr<O, E> {
    #[new(into)]
    expr: Box<E>,
    #[new(into)]
    op: O,
}
// endregion

// region: Arithmetic expressions.
#[from_structs]
#[derive(Debug, PartialEq)]
pub(crate) enum ArithTerm {
    BinExpr(ArithBinExpr),
    Expansion(ArithExpansion),
    Group(ArithGroup),
    TriExpr(ArithTriExpr),
    UnExpr(ArithUnExpr),
    Variable(Variable),
}

pub(crate) type ArithBinExpr = BinExpr<BinOp, ArithTerm>;

#[derive(Debug, New, PartialEq)]
pub(crate) struct ArithExpansion {
    sgmts: Vec<WordSgmt>,
}

#[derive(Debug, New, PartialEq)]
pub(crate) struct ArithGroup {
    seq: ArithSeq,
}

#[derive(Debug, New, PartialEq)]
pub(crate) struct ArithSeq {
    terms: Vec<ArithTerm>,
}

#[derive(Debug, New, PartialEq)]
pub(crate) struct ArithTriExpr {
    #[new(into)]
    cond: Box<ArithTerm>,
    #[new(into)]
    then_branch: Box<ArithTerm>,
    #[new(into)]
    else_branch: Box<ArithTerm>,
}

pub(crate) type ArithUnExpr = UnExpr<UnOp, ArithTerm>;
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

#[from_structs]
#[derive(Debug, PartialEq)]
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

// region: Dollar expressions.
#[from_structs]
#[derive(Debug, PartialEq)]
pub(crate) enum DollarExp {
    Arith(ArithSeq),
    CmdExpansion(DollarCmdExpansion),
    CmdSub(DollarCmdSub),
    DoubleQuoting(DoubleQuoted),
    ParamExpansion(ParamExpansion),
    SingleQuoting(SingleQuoted),
    Variable(Variable),
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

#[derive(Debug, PartialEq)]
pub(crate) struct Cond {
    single_bracketed: bool,
    expr: Option<CondExpr>,
}

impl Cond {
    pub(crate) fn new(single_bracketed: bool) -> Self {
        Self {
            single_bracketed,
            expr: None,
        }
    }

    pub(crate) fn with_expr(single_bracketed: bool, expr: impl Into<CondExpr>) -> Self {
        Self {
            single_bracketed,
            expr: Some(expr.into()),
        }
    }
}

#[from_structs]
#[derive(Debug, PartialEq)]
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
// endregion

// region: Words.
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

#[from_structs]
#[derive(Debug, PartialEq)]
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
pub(crate) struct Glob {
    #[new(into)]
    pattern: String,
}

impl<S: AsRef<str> + ?Sized> PartialEq<S> for Glob {
    fn eq(&self, other: &S) -> bool {
        self.pattern == other.as_ref()
    }
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
// endregion

// region: Redirections.
tokenizable! {
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

#[from_structs]
#[derive(Debug, PartialEq)]
pub(crate) enum FileDesc {
    Number,
    StdOutErr,
    Variable(Variable),
}

#[derive(Debug, New, PartialEq)]
pub(crate) struct Redir {
    file_desc: Option<FileDesc>,
    op: RedirOp,
    word: Word,
}
// endregion

// region: Simple commands.
tokenizable! {
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

#[derive(Debug, New, PartialEq)]
pub(crate) struct Subscript {
    // Left unparsed for now.
    #[new(into)]
    content: String,
}

#[derive(Debug, PartialEq)]
pub(crate) struct Variable {
    ident: String,
    subscripts: Vec<Subscript>,
}

impl Variable {
    pub(crate) fn new(ident: impl Into<String>) -> Self {
        Self::with_subscripts(ident, vec![])
    }

    pub(crate) fn with_subscripts(ident: impl Into<String>, subscripts: Vec<Subscript>) -> Self {
        Self {
            ident: ident.into(),
            subscripts,
        }
    }
}

#[from_structs]
#[derive(Debug, PartialEq)]
pub(crate) enum Value {
    Array(Array),
    Empty,
    KeyValue(KeyValue),
    Word(Word),
}

#[derive(Debug, New, PartialEq)]
pub(crate) struct KeyValue {
    key: Vec<Subscript>,
    #[new(into)]
    value: Box<Value>,
}

#[derive(Debug, New, PartialEq)]
pub(crate) struct Array {
    elems: Vec<Value>,
}

#[derive(Debug, New, PartialEq)]
pub(crate) struct Assign {
    variable: Variable,
    #[new(into)]
    value: Value,
    op: BinOp,
}

#[derive(Debug, New, PartialEq)]
pub(crate) struct SimpleCmd {
    cmd: Option<Word>,
    prefix: Vec<CmdPrefixSgmt>,
    suffix: Vec<CmdSuffixSgmt>,
}

#[from_structs]
#[derive(Debug, PartialEq)]
pub(crate) enum CmdPrefixSgmt {
    Assign(Assign),
    Redir(Redir),
}

#[from_structs]
#[derive(Debug, PartialEq)]
pub(crate) enum CmdSuffixSgmt {
    ArithSeq(ArithSeq),
    Assign(Assign),
    Pipeline(Pipeline),
    Redir(Redir),
    Word(Word),
}

#[from_structs]
#[derive(Debug, PartialEq)]
pub(crate) enum Cmd {
    Compound(CompoundCmd),
    Cond(CondCmd),
    Coproc(Coproc),
    Simple(SimpleCmd),
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

#[derive(Debug, New, PartialEq)]
pub(crate) struct Pipeline {
    cmds: Vec<Cmd>,
}

pub(crate) type List = BinExpr<ControlOp, Term>;

#[from_structs]
#[derive(Debug, PartialEq)]
pub(crate) enum Term {
    List(List),
    Pipeline(Pipeline),
}
// endregion

// region: Compound commands.
#[derive(Debug, New, PartialEq)]
pub(crate) struct CompoundCmd {
    cmd: Construct,
    redirs: Vec<Redir>,
}

#[derive(Debug, PartialEq)]
pub(crate) enum Construct {
    BatsTest(BatsTest),
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

#[derive(Debug, New, PartialEq)]
pub(crate) struct BatsTest {
    #[new(into)]
    name: String,
    #[new(into)]
    body: Term,
}

#[derive(Debug, New, PartialEq)]
pub(crate) struct CaseCmd {
    word: Word,
    clauses: Vec<CaseClause>,
}

#[derive(Debug, New, PartialEq)]
pub(crate) struct CaseClause {
    pattern: Vec<Word>,
    cmd: Option<Term>,
    sep: Option<ClauseSep>,
}

tokenizable! {
    /// Terminators of a `case` clause.
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

#[derive(Debug, New, PartialEq)]
pub(crate) struct CondBlock {
    #[new(into)]
    cond: Term,
    #[new(into)]
    block: Term,
}

#[from_structs]
#[derive(Debug, PartialEq)]
pub(crate) enum ForLoop {
    Arith(ArithForLoop),
    Listed(InListed),
}

#[derive(Debug, New, PartialEq)]
pub(crate) struct ArithForLoop {
    header: (ArithSeq, ArithSeq, ArithSeq),
    body: Term,
}

#[derive(Debug, New, PartialEq)]
pub(crate) struct InListed {
    name: String,
    list: Vec<Word>,
    body: Term,
}

#[derive(Debug, New, PartialEq)]
pub(crate) struct Function {
    name: String,
    body: Term,
}

#[derive(Debug, New, PartialEq)]
pub(crate) struct IfCmd {
    if_branch: CondBlock,
    elif_branches: Vec<CondBlock>,
    else_term: Option<Term>,
}
// endregion
