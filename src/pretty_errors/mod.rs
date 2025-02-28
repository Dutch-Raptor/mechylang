use ariadne::{Cache, Color, Config, LabelAttach, Report, ReportBuilder};
use color_print::cprintln;
use std::io;
use std::ops::Range;

mod error_demo;
mod evaluator_errors;
mod lexer_errors;
mod parser_errors;

pub use error_demo::error_demo;

pub trait PrettyError {
    fn as_pretty_errors<'a>(&self, source_id: &'a str) -> MechyPrettyError<'a, 'a>;
}

pub enum MechyPrettyError<'source_id, 'report> {
    Single(Report<'report, (&'source_id str, Range<usize>)>),
    Multi(MultiError<'source_id, 'report>),
}

pub struct MultiError<'source_id, 'report> {
    pub description: String,
    pub errors: Vec<MechyPrettyError<'source_id, 'report>>,
}

impl<'source_id> MechyPrettyError<'source_id, '_> {
    pub fn eprint<C>(&self, cache: C) -> io::Result<()>
    where
        C: Cache<&'source_id str>,
        C: Clone,
    {
        match self {
            MechyPrettyError::Single(report) => report.eprint(cache),
            MechyPrettyError::Multi(multi_error) => {
                cprintln!(
                    "<r>Multiple related errors ({})</>: {}",
                    multi_error.errors.len(),
                    multi_error.description
                );
                for (index, report) in multi_error.errors.iter().enumerate() {
                    cprintln!("<r>({})</>: ", index + 1);
                    report.eprint(cache.clone())?;
                }
                Ok(())
            }
        }
    }

    pub fn print<C>(&self, cache: C) -> io::Result<()>
    where
        C: Cache<&'source_id str>,
        C: Clone,
    {
        match self {
            MechyPrettyError::Single(report) => report.print(cache),
            MechyPrettyError::Multi(multi_error) => {
                for (index, report) in multi_error.errors.iter().enumerate() {
                    cprintln!("<r>({})</>: ", index + 1);
                    report.print(cache.clone())?;
                }
                Ok(())
            }
        }
    }

    pub fn write<C, W>(&self, cache: C, writer: &mut W) -> io::Result<()>
    where
        C: Cache<&'source_id str>,
        C: Clone,
        W: io::Write,
    {
        match self {
            MechyPrettyError::Single(report) => report.write(cache, writer),
            MechyPrettyError::Multi(multi_error) => {
                for (index, report) in multi_error.errors.iter().enumerate() {
                    cprintln!("<r>({})</>: ", index + 1);
                    report.write(cache.clone(), writer)?;
                }
                Ok(())
            }
        }
    }
}

pub enum MechyPrettyErrorBuilder<'source_id> {
    Single(ReportBuilder<'source_id, (&'source_id str, Range<usize>)>),
    Multi(MultiErrorBuilder<'source_id>),
}

pub struct MultiErrorBuilder<'source_id> {
    pub description: String,
    pub errors: Vec<MechyPrettyErrorBuilder<'source_id>>,
}

struct HighlightColors {
    colors: [Color; 3],
    index: usize,
    color_generator: ariadne::ColorGenerator,
}

impl HighlightColors {
    fn new() -> Self {
        Self {
            // Credit https://github.com/zkat/miette/blob/5f441d011560a091fe5d6a6cdb05f09acf622d36/src/handlers/theme.rs#L112
            colors: [
                Color::Rgb(246, 87, 248),
                Color::Rgb(30, 201, 212),
                Color::Rgb(145, 246, 111),
            ],
            index: 0,
            color_generator: ariadne::ColorGenerator::new(),
        }
    }

    fn next(&mut self) -> Color {
        if self.index >= self.colors.len() {
            self.color_generator.next()
        } else {
            let color = self.colors[self.index];
            self.index += 1;
            color
        }
    }
}

fn highlight_colors() -> HighlightColors {
    HighlightColors::new()
}

const KEYWORD_COLOR: Color = Color::Fixed(81);

impl<T: PrettyErrorBuilder> PrettyError for T
where
    T: Sized,
{
    fn as_pretty_errors<'a>(&self, source_id: &'a str) -> MechyPrettyError<'a, 'a> {
        match self.as_pretty_error_builder(source_id) {
            MechyPrettyErrorBuilder::Single(report_builder) => MechyPrettyError::Single(
                report_builder
                    .with_config(
                        Config::default()
                            .with_index_type(ariadne::IndexType::Byte)
                            .with_label_attach(LabelAttach::Start),
                    )
                    .finish(),
            ),
            MechyPrettyErrorBuilder::Multi(multi_error_builder) => {
                fn as_pretty_error(builder: MechyPrettyErrorBuilder) -> MechyPrettyError {
                    match builder {
                        MechyPrettyErrorBuilder::Single(report_builder) => {
                            MechyPrettyError::Single(report_builder.finish())
                        }
                        MechyPrettyErrorBuilder::Multi(multi_error_builder) => {
                            MechyPrettyError::Multi(MultiError {
                                description: multi_error_builder.description,
                                errors: multi_error_builder
                                    .errors
                                    .into_iter()
                                    .map(|report_builder| as_pretty_error(report_builder))
                                    .collect(),
                            })
                        }
                    }
                }

                MechyPrettyError::Multi(MultiError {
                    description: multi_error_builder.description,
                    errors: multi_error_builder
                        .errors
                        .into_iter()
                        .map(|report_builder| as_pretty_error(report_builder))
                        .collect(),
                })
            }
        }
    }
}

trait PrettyErrorBuilder {
    fn as_pretty_error_builder<'a>(&self, source_id: &'a str) -> MechyPrettyErrorBuilder<'a>;
}

impl PrettyErrorBuilder for crate::Error {
    fn as_pretty_error_builder<'a>(&self, source_id: &'a str) -> MechyPrettyErrorBuilder<'a> {
        match self {
            crate::Error::LexerError(err) => err.as_pretty_error_builder(source_id),
            crate::Error::ParserError(err) => err.as_pretty_error_builder(source_id),
            crate::Error::EvaluatorError(err) => err.as_pretty_error_builder(source_id),
        }
    }
}
