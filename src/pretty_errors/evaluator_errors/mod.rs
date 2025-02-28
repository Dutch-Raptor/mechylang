use crate::evaluator::{Error, IntoIteratorError};
use crate::pretty_errors::{
    highlight_colors, MechyPrettyErrorBuilder, PrettyErrorBuilder, KEYWORD_COLOR,
};
use crate::support::tap::TapOwned;
use ariadne::{Color, Fmt, Label, Report, ReportKind};
use itertools::Itertools;

impl PrettyErrorBuilder for Error {
    fn as_pretty_error_builder<'a>(&self, source_id: &'a str) -> MechyPrettyErrorBuilder<'a> {
        let mut color_generator = highlight_colors();
        match self {
            Error::LexerError(err) => err.as_pretty_error_builder(source_id),
            Error::ParserError(err) => err.as_pretty_error_builder(source_id),
            Error::UnsupportedInfixOperator {
                left_span,
                right_span,
                left_object,
                right_object,
                operator,
                operator_span,
            } => {
                let type1_color = color_generator.next();
                let type2_color = color_generator.next();
                let operator_color = color_generator.next();

                MechyPrettyErrorBuilder::Single(Report::build(ReportKind::Error, (source_id, left_span.bytes()))
                    .with_message("Incompatible types for infix operator")
                    .with_label(Label::new((source_id, left_span.bytes.clone())).with_message(
                        format!("this has type `{type}`",
                                type = left_object.get_type().fg(type1_color))).with_color(type1_color))
                    .with_label(Label::new((source_id, right_span.bytes.clone())).with_message(
                        format!("this has type `{type}`", type = right_object.get_type().fg(type2_color))).with_color(type2_color))
                    .with_label(Label::new((source_id, operator_span.bytes.clone())).with_message(
                        format!("`{operator}` is not defined for these types",
                                operator = operator.fg(operator_color))).with_color(operator_color)
                        .with_order(1)
                    )
                    .with_help("Use type conversion or a compatible operation.".to_string()))
            }
            Error::TypeError {
                span,
                expected,
                found,
                context,
            } => {
                let found_type_color = color_generator.next();
                let expected_type_color = color_generator.next();

                let expected = if expected.len() == 1 {
                    expected[0].to_string()
                } else {
                    format!(
                        "one of {}",
                        expected
                            .iter()
                            .map(ToString::to_string)
                            .join(", ")
                            .fg(expected_type_color)
                    )
                };

                MechyPrettyErrorBuilder::Single(
                    Report::build(ReportKind::Error, (source_id, span.bytes()))
                        .with_message("Type error")
                        .with_label(
                            Label::new((source_id, span.bytes.clone()))
                                .with_message(format!("this has type `{type}`",
                                                  type = found.fg(found_type_color)))
                                .with_color(found_type_color)
                                .with_priority(1),
                        )
                        .with_label(
                            Label::new((source_id, span.bytes.clone()))
                                .with_message(format!(
                                    "but expected type {expected}",
                                    expected = &expected
                                ))
                                .with_color(expected_type_color),
                        )
                        .tap_owned(|mut report| {
                            if let Some(context) = context {
                                report = report.with_label(
                                    Label::new((source_id, context.bytes.clone()))
                                        .with_message("in this context".to_string())
                                        .with_priority(0)
                                        .with_order(1000),
                                );
                            }
                            report
                        }),
                )
            }
            Error::AssertionEqualFailed {
                first_span,
                second_span,
                first_value,
                second_value,
                assert_span,
            } => {
                let first_value_color = color_generator.next();
                let second_value_color = color_generator.next();
                MechyPrettyErrorBuilder::Single(
                    Report::build(ReportKind::Error, (source_id, first_span.bytes()))
                        .with_message("Assertion failed".to_string())
                        .with_label(
                            Label::new((source_id, first_span.bytes.clone()))
                                .with_message(format!(
                                    "this value `{value}`",
                                    value = first_value.fg(first_value_color)
                                ))
                                .with_color(first_value_color)
                                .with_order(0)
                                .with_priority(1),
                        )
                        .with_label(
                            Label::new((source_id, second_span.bytes.clone()))
                                .with_message(format!(
                                    "is not equal to `{value}`",
                                    value = second_value.fg(second_value_color)
                                ))
                                .with_color(second_value_color)
                                .with_order(1)
                                .with_priority(1),
                        )
                        .with_label(
                            Label::new((source_id, assert_span.bytes.clone()))
                                .with_message("in this assertion")
                                .with_order(2)
                                .with_priority(0),
                        ),
                )
            }

            Error::InvalidPrefixOperatorForType {
                span,
                operator,
                right,
                operator_span,
                right_span,
            } => {
                let type1_color = color_generator.next();
                let operator_color = color_generator.next();

                MechyPrettyErrorBuilder::Single(
                    Report::build(ReportKind::Error, (source_id, span.bytes()))
                        .with_message(format!(
                            "Operator `{operator}` is not defined for {type_name}",
                            operator = operator,
                            type_name = right.get_type(),
                        ))
                        .with_label(
                            Label::new((source_id, operator_span.bytes.clone()))
                                .with_message(format!(
                                    "`{operator}` cannot be applied to a {type_name}",
                                    operator = operator.fg(operator_color),
                                    type_name = right.get_type().fg(type1_color)
                                ))
                                .with_color(operator_color)
                                .with_priority(1),
                        )
                        .with_label(
                            Label::new((source_id, right_span.bytes.clone()))
                                .with_message(format!("This has type `{type}`",
                                                  type = right.get_type().fg(type1_color)))
                                .with_color(type1_color)
                                .with_priority(0)
                                .with_order(1),
                        ),
                )
            }

            Error::IdentifierNotFound {
                span,
                identifier,
                similar,
            } => MechyPrettyErrorBuilder::Single(
                Report::build(ReportKind::Error, (source_id, span.bytes()))
                    .with_message(format!(
                        "Undefined identifier `{identifier}`",
                        identifier = identifier
                    ))
                    .with_label(
                        Label::new((source_id, span.bytes.clone()))
                            .with_message(format!(
                                "`{identifier}` is not defined in this scope",
                                identifier = identifier.fg(Color::Red)
                            ))
                            .with_color(Color::Red)
                            .with_priority(0)
                            .with_order(1),
                    )
                    .tap_owned(|report| {
                        if let Some(sim) = similar {
                            report.with_help(format!(
                                "A variable with a similar name exists: `{sim}`",
                                sim = sim.fg(color_generator.next())
                            ))
                        } else {
                            report
                        }
                    }),
            ),

            Error::WrongNumberOfArguments {
                span,
                unexpected_arg,
                expected,
                found,
            } => {
                let (expected, expected_plural) = if expected.start() != expected.end() {
                    (format!("{expected:?}"), "s")
                } else {
                    (
                        format!("{expected}", expected = expected.start()),
                        if *expected.start() == 1 { "" } else { "s" },
                    )
                };

                let found_plural_was = if *found != 1 { "were" } else { "was" };

                MechyPrettyErrorBuilder::Single(Report::build(ReportKind::Error, (source_id, span.bytes()))
                    .with_message(format!("This function takes {expected} argument{expected_plural}, but {found} {found_plural_was} provided"))
                    .with_label(
                        Label::new((source_id, span.bytes.clone()))
                            .with_message(format!("{expected} expected argument{expected_plural}"))
                            .with_priority(0)
                            .with_order(0))
                    .tap_owned(|report|
                        if let Some(unexpected_arg) = unexpected_arg {
                            report.with_label(
                                Label::new((source_id, unexpected_arg.1.span.clone().unwrap_or_default().bytes))
                                    .with_message(format!("Unexpected argument #{} of type `{type}`",
                                                          unexpected_arg.0,
                                                          type = unexpected_arg.1.value.get_type().fg(Color::Red)))
                                    .with_priority(1)
                                    .with_color(Color::Red)
                                    .with_order(1))
                        } else {
                            report
                        }))
            }

            Error::CannotCall { function, span } => {
                let function_color = color_generator.next();
                MechyPrettyErrorBuilder::Single(
                    Report::build(ReportKind::Error, (source_id, span.bytes()))
                        .with_message(format!(
                            "Cannot call `{function}`",
                            function = function.fg(function_color)
                        ))
                        .with_label(
                            Label::new((source_id, span.bytes.clone()))
                                .with_message(format!(
                                    "`{function}` is not a function",
                                    function = function.fg(function_color)
                                ))
                                .with_color(function_color)
                                .with_priority(0)
                                .with_order(1),
                        ),
                )
            }

            Error::IterCallOnNonIterable {
                obj_span,
                iter_call_span,
                reason,
                obj,
            } => {
                let type_color = color_generator.next();
                MechyPrettyErrorBuilder::Single(Report::build(ReportKind::Error, (source_id, iter_call_span.bytes()))
                    .with_message(
                        match reason {
                            IntoIteratorError::InvalidRange =>
                                format!("Cannot iterate over `{type}` range",
                                        type = obj.get_type().fg(type_color)),
                            IntoIteratorError::NotAnIterable =>
                                format!("`{type}` is not an iterable type",
                                        type = obj.get_type().fg(type_color)),
                        }
                    )
                    .with_label(
                        Label::new((source_id, obj_span.bytes.clone()))
                            .with_message(format!("Cannot iterate over `{type}`",
                                                  type = obj.get_type().fg(type_color))).with_color(type_color)
                            .with_priority(0)
                            .with_order(1))
                    .with_label(
                        Label::new((source_id, iter_call_span.bytes.clone()))
                            .with_message(format!("in this call to `{iter}`",
                                                  iter = "iter".fg(Color::Red)))
                            .with_color(Color::Red)
                            .with_priority(1)
                            .with_order(2)
                    )
                    .tap_owned(|report|
                        if let IntoIteratorError::InvalidRange = reason {
                            report.with_help("Ranges can only iterate over integer values")
                        } else {
                            report.with_help(
                                format!("Iteration only works on collections, ranges, and strings - not on primitive types like `{type}`",
                                        type = obj.get_type().fg(type_color)))
                        }))
            }

            Error::IteratorMethodOnIterable {
                method_span,
                method_name,
                object_type,
                object_span,
            } => {
                let method_name_color = color_generator.next();
                let object_type_color = color_generator.next();
                let iter_call_color = color_generator.next();
                MechyPrettyErrorBuilder::Single(Report::build(ReportKind::Error, (source_id, method_span.bytes()))
                    .with_message(format!("Method `{method_name}` is not defined for `{type}` (an iterable type)`",
                                          method_name = method_name.fg(method_name_color),
                                          type = object_type.fg(object_type_color)
                    ))
                    .with_label(
                        Label::new((source_id, method_span.bytes.clone()))
                            .with_message(format!("`{method_name}` is not defined for `{type}`",
                                                  method_name = method_name.fg(method_name_color),
                                                  type = object_type.fg(object_type_color))).with_color(method_name_color)
                            .with_priority(0)
                            .with_order(1))
                    .with_label(
                        Label::new((source_id, object_span.bytes.clone()))
                            .with_message(format!("`{type}` is an {iterable}, but not an {iterator}",
                                                  type = object_type.fg(object_type_color),
                                                  iterable = "iterable".fg(object_type_color),
                                                  iterator = "iterator".fg(object_type_color)
                            )).with_color(object_type_color)
                            .with_priority(0)
                            .with_order(2))
                    .with_help(format!("`{type}` is an iterable type, to use `{method_name}` first convert it to an iterator using `{iter_call}`",
                                       type = object_type.fg(object_type_color),
                                       method_name = method_name.fg(method_name_color),
                                       iter_call = "iter()".fg(iter_call_color)
                    )))
            }
            Error::IteratingOverNonIterable {
                obj_span,
                reason,
                obj,
                for_span,
            } => {
                let type_color = color_generator.next();
                MechyPrettyErrorBuilder::Single(Report::build(ReportKind::Error, (source_id, obj_span.bytes()))
                    .with_message(
                        match reason {
                            IntoIteratorError::InvalidRange =>
                                format!("Cannot iterate over `{type}` range",
                                        type = obj.get_type().fg(type_color)),
                            IntoIteratorError::NotAnIterable =>
                                format!("`{type}` is not an iterable type",
                                        type = obj.get_type().fg(type_color)),
                        })
                    .with_label(
                        Label::new((source_id, obj_span.bytes.clone()))
                            .with_message(format!("Cannot iterate over `{type}`",
                                                  type = obj.get_type().fg(type_color)
                            ))
                            .with_color(type_color)
                            .with_priority(0)
                            .with_order(1))
                    .with_label(
                        Label::new((source_id, for_span.bytes.clone()))
                            .with_message(format!("in this {for} loop",
                                                  for = "for".fg(KEYWORD_COLOR)))
                            .with_priority(0)
                            .with_order(2)
                    )
                    .with_help(match reason {
                        IntoIteratorError::InvalidRange =>
                            "Ranges can only iterate over integer values".to_string(),
                        IntoIteratorError::NotAnIterable =>
                            format!("Iteration only works on collections, ranges, and strings - not on primitive types like `{type}`",
                                    type = obj.get_type().fg(type_color)),
                    }))
            }
            Error::MutateNonExistentVariable { name } => MechyPrettyErrorBuilder::Single(
                Report::build(ReportKind::Error, (source_id, 0..0))
                    .with_message(format!("Cannot mutate non-existent variable `{name}`")),
            ),
            Error::IndexOutOfBounds {
                array_span,
                index_span,
                index,
                length,
            } => MechyPrettyErrorBuilder::Single(
                Report::build(ReportKind::Error, (source_id, index_span.bytes()))
                    .with_message(format!(
                        "Index `{index}` is out of bounds for array of length `{length}`"
                    ))
                    .with_label(
                        Label::new((source_id, array_span.bytes.clone()))
                            .with_message(format!("This array has length `{length}`"))
                            .with_priority(0)
                            .with_order(1),
                    )
                    .with_label(
                        Label::new((source_id, index_span.bytes.clone()))
                            .with_message(format!("Index `{index}` is out of bounds"))
                            .with_color(Color::Red)
                            .with_priority(0)
                            .with_order(2),
                    ),
            ),
            Error::PropertyNotFound {
                property,
                span,
                object_type,
            } => MechyPrettyErrorBuilder::Single(
                Report::build(ReportKind::Error, (source_id, span.bytes()))
                    .with_message(format!("Property `{}` not found", property))
                    .with_label(
                        Label::new((source_id, span.bytes.clone()))
                            .with_message(format!(
                                "Object of type `{}` has no property `{}`",
                                object_type, property
                            ))
                            .with_color(Color::Red)
                            .with_priority(0)
                            .with_order(1),
                    ),
            ),
            Error::InvalidIndexedAssignmentExpression { span, left } => {
                MechyPrettyErrorBuilder::Single(
                    Report::build(ReportKind::Error, (source_id, span.bytes()))
                        .with_message(format!("Cannot assign to indexed expression `{}`", left))
                        .with_label(
                            Label::new((source_id, span.bytes.clone()))
                                .with_message(format!("Cannot assign to an index of `{}`", left))
                                .with_color(Color::Red)
                                .with_priority(0)
                                .with_order(1),
                        ),
                )
            }
            Error::MutateError { span, name } => MechyPrettyErrorBuilder::Single(
                Report::build(ReportKind::Error, (source_id, span.bytes()))
                    .with_message(format!("Cannot mutate `{}`", name))
                    .with_label(
                        Label::new((source_id, span.bytes.clone()))
                            .with_message(format!("Cannot mutate `{}`", name))
                            .with_color(Color::Red)
                            .with_priority(0)
                            .with_order(1),
                    ),
            ),

            Error::BuiltInError {
                span,
                message,
                error_type,
            } => MechyPrettyErrorBuilder::Single(
                Report::build(ReportKind::Error, (source_id, span.bytes()))
                    .with_message(format!("{:?}: {}", error_type, message))
                    .with_label(
                        Label::new((source_id, span.bytes.clone()))
                            .with_message(message)
                            .with_color(Color::Red)
                            .with_priority(0)
                            .with_order(1),
                    ),
            ),
            Error::AssertionFailed {
                span,
                assert_span,
                value,
            } => MechyPrettyErrorBuilder::Single(
                Report::build(ReportKind::Error, (source_id, span.bytes()))
                    .with_message(format!("Assertion failed"))
                    .with_label(
                        Label::new((source_id, span.bytes.clone()))
                            .with_message(format!("This expression evaluates to `{value}`"))
                            .with_color(Color::Red)
                            .with_priority(0)
                            .with_order(1),
                    )
                    .with_label(
                        Label::new((source_id, assert_span.bytes.clone()))
                            .with_message("in this assertion")
                            .with_priority(0)
                            .with_order(2),
                    ),
            ),
            Error::IOError { error, span } => MechyPrettyErrorBuilder::Single(
                Report::build(
                    ReportKind::Error,
                    (
                        source_id,
                        span.as_ref().map(|span| span.bytes()).unwrap_or_default(),
                    ),
                )
                .with_message(format!("IO error: {:?}", error))
                .with_label(
                    Label::new((source_id, span.clone().unwrap_or_default().bytes))
                        .with_message(format!("IO error: {:?}", error))
                        .with_color(Color::Red)
                        .with_priority(0)
                        .with_order(1),
                ),
            ),
            Error::IndexingNonIndexableType {
                indexed_span,
                indexed_obj,
            } => MechyPrettyErrorBuilder::Single(
                Report::build(ReportKind::Error, (source_id, indexed_span.bytes()))
                    .with_message(format!("Cannot index type `{}`", indexed_obj.get_type()))
                    .with_label(
                        Label::new((source_id, indexed_span.bytes.clone()))
                            .with_message(format!("Cannot index `{}`", indexed_obj))
                            .with_color(Color::Red)
                            .with_priority(0)
                            .with_order(1),
                    ), // e => Report::build(ReportKind::Error, source_id, 0).with_message(format!("{}", e))
            ),
        }
    }
}
