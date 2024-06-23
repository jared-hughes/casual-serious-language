use std::fmt;

pub trait Diagnostic {
    /// Write out as a diagnostic out of `DiagCtxt`.
    #[must_use]
    fn into_diag(self) -> Diag;
}

#[must_use]
#[derive(Clone)]
pub struct Diag {
    // TODO: code span, severity, suggestions
    pub message: String,
}

impl fmt::Debug for Diag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", &self.message)
    }
}
