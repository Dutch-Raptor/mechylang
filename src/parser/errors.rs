use crate::{Error, Span, trace};
use crate::error::ErrorKind;
use crate::parser::Parser;

impl Parser {
    pub(crate) fn error(
        &self,
        kind: ErrorKind,
        msg: impl ToString,
        span: Span,
        context: Option<String>,
    ) -> Error {
        let error = Error::new(kind, msg, span, &self.lines, context);
        trace!(format!("Error: {:?}", error).as_str());
        error
    }
    
    pub fn error_span(&self, kind: ErrorKind, msg: impl ToString, span: Span) -> Error {
        let error = Error::new(kind, msg, span, &self.lines, None);
        trace!(format!("Error: {:?}", error).as_str());
        error
    }

    pub(crate) fn error_current(&self, kind: ErrorKind, msg: impl ToString) -> Error {
        self.error(kind, msg, self.cur_token.span.clone(), None)
    }

    pub(crate) fn error_current_with_context(&self, kind: ErrorKind, msg: impl ToString, context: String) -> Error {
        self.error(
            kind,
            msg,
            self.cur_token.span.clone(),
            Some(context),
        )
    }

    pub(crate) fn error_peek(&self, kind: ErrorKind, msg: impl ToString) -> Error {
        self.error(
            kind,
            msg,
            self.peek_token.span.clone(),
            None,
        )
    }

    #[allow(dead_code)]
    pub(crate) fn error_peek_with_context(&self, kind: ErrorKind, msg: impl ToString, context: String) -> Error {
        self.error(
            kind,
            msg,
            self.peek_token.span.clone(),
            Some(context),
        )
    }
}