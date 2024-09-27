use crate::evaluator::Error;
use crate::pretty_errors::PrettyError;

impl PrettyError for Error {
    fn as_pretty_error(&self) -> miette::Report {
        match self {
            Error::LexerError(err) => err.as_pretty_error(),
            Error::ParserError(err) => err.as_pretty_error(),
            Error::TypeError { span, expected, found } => {
                miette::miette!(
                    code = "evaluator::TypeError",
                    labels = vec![
                        miette::LabeledSpan::new(Some("this expression".to_string()), span.start(), span.length())
                    ],
                    help = "Check that the expression has the correct type",
                    "Type error. Expected {}, found {}",
                    expected.iter().map(|t| t.to_string()).collect::<Vec<String>>().join(", "),
                    found,
                )
            }
            Error::UnsupportedInfixOperator {
                left_span,
                right_span,
                left_object,
                right_object,
                operator,
                operator_span
            } => {
                miette::miette!(
                    code = "evaluator::UnsupportedInfixOperator",
                    labels = vec![
                        miette::LabeledSpan::new(
                            Some(format!("`{}` is not defined for these types", operator)),
                            operator_span.start(),
                            operator_span.length()
                        ),
                        miette::LabeledSpan::new(
                            Some(format!("This is a `{}`", left_object.get_type())),
                            left_span.start(),
                            left_span.length()
                        ),
                        miette::LabeledSpan::new(
                            Some(format!("This is a `{}`", right_object.get_type())),
                            right_span.start(),
                            right_span.length()
                        ),
                    ],
                    help = "Ensure the types are compatible or use an appropriate method to convert the types",
                    "Cannot use `{}` on types `{}` and `{}`",
                    operator,
                    left_object.get_type(),
                    right_object.get_type(),
                )
            }
            _ => miette::miette!("Unknown evaluator error"),
        }
    }
}
