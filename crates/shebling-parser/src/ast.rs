use shebling_codegen::{from_structs, New};

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
