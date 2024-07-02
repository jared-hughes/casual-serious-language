use crate::span::Span;
use std::fmt;

pub(crate) trait Diagnostic {
    fn into_diag(self) -> Diag;
}

#[must_use]
#[derive(Clone)]
pub struct Diag {
    pub(crate) message: String,
    pub(crate) span: Span,
}

impl fmt::Debug for Diag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "At {:?}: {}", self.span, &self.message)
    }
}

macro_rules! def_errors {
    ($(
        pub(crate) struct $Err:ident $def:tt
        msg: $self:ident => $msg:expr;
    )+) => {$(
        pub(crate) struct $Err $def
        impl Diagnostic for $Err {
            fn into_diag($self) -> Diag {
                Diag {
                    span: $self.span,
                    message: $msg,
                }
            }
        }
    )+};
}

pub(crate) use def_errors;

/// Intended for parser only.
/// Errors to pass into consume_ident!
macro_rules! def_token_errors {
    ($(
        pub(crate) struct $Err:ident $def:tt
        msg: $self:ident => $msg:expr;
    )+) => {$(
        pub(crate) struct $Err $def
        impl $Err {
            // Only used via a macro, which Clippy doesn't find.
            #[allow(unused)]
            pub(crate) fn span(self, span: Span) -> Spanned<$Err> {
                Spanned {node: self, span}
            }
            fn msg($self) -> String {
                $msg
            }
        }
        impl Diagnostic for Spanned<$Err> {
            fn into_diag(self) -> Diag {
                Diag {
                    span: self.span,
                    message: self.node.msg(),
                }
            }
        }
    )+};

    ($(
        $Err:ident => $msg:expr,
    )+) => {$(
        pub(crate) struct $Err;
        impl $Err {
            pub(crate) fn span(self, span: Span) -> Spanned<$Err> {
                Spanned {node: self, span}
            }
            fn msg(self) -> String {
                $msg
            }
        }
        impl Diagnostic for Spanned<$Err> {
            fn into_diag(self) -> Diag {
                Diag {
                    span: self.span,
                    message: self.node.msg(),
                }
            }
        }
    )+};
}

pub(crate) use def_token_errors;
