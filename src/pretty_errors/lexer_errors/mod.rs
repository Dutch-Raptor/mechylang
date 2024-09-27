use miette::LabeledSpan;
use crate::lexer::Error;
use crate::pretty_errors::PrettyError;

impl PrettyError for Error {
    fn as_pretty_error(&self) -> miette::Report {
        match self {
            Error::IllegalCharacter { span, found } => {
                miette::miette!(
                    labels = vec![
                        LabeledSpan::new(Some("this character".to_string()), span.start(), span.length())
                    ],
                    "Invalid character '{}' found", found,
                )
            }
            _ => miette::miette!("Unknown lexer error"),
        }
    }
}
