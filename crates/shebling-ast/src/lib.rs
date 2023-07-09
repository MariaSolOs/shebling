/// Trait for types that represent single tokens, such as keywords
/// and operators.
pub trait Token
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
            pub enum $name {
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

mod expansion;
mod expression;
mod redirection;
mod span;
mod word;

pub use expansion::*;
pub use expression::*;
pub use redirection::*;
pub use span::*;
pub use word::*;
