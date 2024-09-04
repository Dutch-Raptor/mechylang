use std::fmt::{Debug, Formatter};

pub struct EvalConfig {
    pub output_fn: Box<dyn Fn(String) + Send>,
}

impl Debug for EvalConfig {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "EvalConfig")
    }
}

impl Default for EvalConfig {
    fn default() -> Self {
        Self {
            output_fn: Box::new(|string| print!("{}", string)),
        }
    }
}

impl EvalConfig {
    pub fn new() -> Self {
        Self::default()
    }

    /// If you want to capture the output of the evaluator, you can use this function.
    ///
    /// This closure will be called every time the evaluator prints something.
    /// The string passed to the closure is the string that would be printed.
    ///
    /// # Examples
    /// ```
    /// use mechylang::EvalConfig;
    /// use mechylang::Evaluator;
    /// use std::sync::Arc;
    /// use std::sync::Mutex;
    ///
    /// let (sender, receiver) = std::sync::mpsc::channel();
    ///
    /// let config = EvalConfig::new().with_output(move |string| {
    ///    sender.send(string).unwrap();
    ///    // You can also do other things with the string here, like write it to a file.
    /// });
    ///
    /// Evaluator::eval("print(\"Hello, world!\")", &mut Default::default(), config).unwrap();
    ///
    /// assert_eq!(receiver.recv().unwrap(), "Hello, world!");
    /// ```
    pub fn with_output(mut self, output_fn: impl Fn(String) + Send + 'static) -> Self {
        self.output_fn = Box::new(output_fn);
        self
    }
}
