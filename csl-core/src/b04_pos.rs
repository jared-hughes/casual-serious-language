use std::fmt;
use std::ops::{Add, Sub};

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub(crate) struct BytePos(pub(crate) u32);

impl From<usize> for BytePos {
    fn from(x: usize) -> Self {
        BytePos(x.try_into().expect("File should be less than 4 GB."))
    }
}

impl Sub for BytePos {
    type Output = ByteLen;

    #[inline(always)]
    fn sub(self, rhs: BytePos) -> ByteLen {
        ByteLen(self.0 - rhs.0)
    }
}

impl Sub<ByteLen> for BytePos {
    type Output = BytePos;

    #[inline(always)]
    fn sub(self, rhs: ByteLen) -> BytePos {
        BytePos(self.0 - rhs.0)
    }
}

impl Add<ByteLen> for BytePos {
    type Output = BytePos;

    #[inline(always)]
    fn add(self, rhs: ByteLen) -> BytePos {
        BytePos(self.0 + rhs.0)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub(crate) struct ByteLen(pub(crate) u32);

impl From<usize> for ByteLen {
    fn from(x: usize) -> Self {
        ByteLen(x.try_into().expect("File should be less than 4 GB."))
    }
}

impl From<ByteLen> for u32 {
    fn from(x: ByteLen) -> Self {
        x.0
    }
}

impl Sub for ByteLen {
    type Output = ByteLen;

    #[inline(always)]
    #[mutants::skip] // Unused but good for reference
    fn sub(self, rhs: ByteLen) -> ByteLen {
        ByteLen(self.0 - rhs.0)
    }
}

impl Add for ByteLen {
    type Output = ByteLen;

    #[inline(always)]
    #[mutants::skip] // Unused but good for reference
    fn add(self, rhs: ByteLen) -> ByteLen {
        ByteLen(self.0 + rhs.0)
    }
}

impl fmt::Display for BytePos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // 1-indexed positions.
        write!(f, "{}", self.0 + 1)
    }
}
