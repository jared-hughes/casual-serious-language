use crate::pos::{ByteLen, BytePos};
use std::fmt;

/// A half-open span: (lo..hi) including lo and excluding hi.
#[derive(Clone, Copy)]
pub struct Span {
    pub lo: BytePos,
    pub hi: BytePos,
}

pub const DUMMY_SPAN: Span = Span {
    lo: BytePos(0),
    hi: BytePos(0),
};

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.hi - self.lo == ByteLen(1) {
            write!(f, "{}", self.lo)
        } else if self.hi - self.lo <= ByteLen(0) {
            write!(f, "(!{},{}!)", self.lo, self.hi)
        } else {
            write!(f, "{}-{}", self.lo, self.hi - ByteLen(1))
        }
    }
}

#[derive(Clone, Copy)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

impl<T: fmt::Debug> fmt::Debug for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}({:?})", self.node, self.span)
    }
}

pub fn respan<T>(t: T, sp: Span) -> Spanned<T> {
    Spanned { node: t, span: sp }
}

pub fn span(lo: BytePos, hi: BytePos) -> Span {
    Span { lo, hi }
}
