use miette::{Diagnostic, NamedSource};

mod lexer_errors;
mod parser_errors;
mod evaluator_errors;

pub trait PrettyError {
    fn as_pretty_error(&self) -> miette::Report;

    fn as_pretty_error_with_source_code(&self, source_code: String) -> miette::Report {
        self.as_pretty_error().with_source_code(source_code)
    }

    fn as_pretty_with_named_source(&self, name: String, source_code: String) -> miette::Report {
        NamedSourceCodeError {
            errors: vec![self.as_pretty_error()],
            src: NamedSource::new(name, source_code),
        }.into()
    }
}

#[derive(Debug, Diagnostic)]
struct NamedSourceCodeError {
    #[related]
    pub errors: Vec<miette::Report>,
    #[source_code]
    pub src: NamedSource<String>,
}

impl std::fmt::Display for NamedSourceCodeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Some errors occurred while running the code")
    }
}

impl std::error::Error for NamedSourceCodeError {}

impl PrettyError for crate::Error {
    fn as_pretty_error(&self) -> miette::Report {
        match self {
            crate::Error::LexerError(err) => err.as_pretty_error(),
            crate::Error::ParserError(err) => err.as_pretty_error(),
            crate::Error::EvaluatorError(err) => err.as_pretty_error(),
        }
    }
}

#[derive(Debug, Diagnostic)]
struct MultiError {
    #[related]
    pub errors: Vec<miette::Report>,
    pub error: String,
}

impl std::fmt::Display for MultiError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.error)
    }
}

impl std::error::Error for MultiError {}


