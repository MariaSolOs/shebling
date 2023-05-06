use shebling_codegen::{from_structs, New};

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

// region: Shell expansions.
#[from_structs]
#[derive(Debug, PartialEq)]
pub(crate) enum DollarExp {
    Arith(ArithSeq),
    // TODO CmdExpansion(DollarCmdExpansion),
    // TODO CmdSub(DollarCmdSub),
    // TODO DoubleQuoting(DoubleQuoted),
    ParamExpansion(ParamExpansion),
    // TODO SingleQuoting(SingleQuoted),
    Variable(Variable),
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
    // TODO BackQuoted(BackQuoted),
    Lit(Lit),
    DollarExp(DollarExp),
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
    // TODO BackQuoted(BackQuoted),
    BraceExpansion(BraceExpansion),
    DollarExp(DollarExp),
    DoubleQuoted(DoubleQuoted),
    Glob(Glob),
    Lit(Lit),
    // TODO ProcSub(ProcSub),
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

#[derive(Debug, New, PartialEq)]
pub(crate) struct Subscript {
    // Left unparsed for now.
    #[new(into)]
    content: String,
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

#[derive(Debug, PartialEq)]
pub(crate) struct Cond {
    single_bracketed: bool,
    expr: Option<CondExpr>,
}

impl Cond {
    pub(crate) fn empty(single_bracketed: bool) -> Self {
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

// region: Commands.
#[derive(Debug, New, PartialEq)]
pub(crate) struct CondCmd {
    cond: Cond,
    redirs: Vec<Redir>,
}
// endregion

// region: Command sequences.
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

// endregion
