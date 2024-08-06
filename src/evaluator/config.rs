pub struct EvalConfig {
    pub output_fn: Box<dyn Fn(String) + Send>,
    pub print_tokens: bool,
    pub print_ast: bool,
}

impl Default for EvalConfig {
    fn default() -> Self {
        Self {
            output_fn: Box::new(|string| print!("{}", string)),
            print_tokens: false,
            print_ast: false,
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
    
    pub fn with_print_tokens(mut self, print_tokens: bool) -> Self {
        self.print_tokens = print_tokens;
        self
    }
    
    pub fn with_print_ast(mut self, print_ast: bool) -> Self {
        self.print_ast = print_ast;
        self
    }
}
