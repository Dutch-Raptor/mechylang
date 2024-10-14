use std::sync::Arc;
use lazy_static::lazy_static;
use crate::evaluator::Error;
use crate::evaluator::objects::{ArgumentList, FunctionTy, MethodTy, ObjectTy};
use crate::Object;

use super::MethodInner;

lazy_static! {
    /// ## `not() -> Boolean`
    /// Returns the opposite of the boolean
    ///
    /// ```rust
    /// # mechylang::test_utils::test_eval_ok(r#"
    /// assert_eq(true.not(), false);
    /// assert_eq(false.not(), true);
    /// # "#);
    /// ```
    pub static ref BOOLEAN_METHODS: Arc<[MethodInner]> = Arc::new([MethodInner {
        method_ty: MethodTy {
            method_name: "not",
            self_ty: Box::new(ObjectTy::Boolean),
            function_ty: FunctionTy {
                arguments: ArgumentList::None,
                expected_return_type: Some(Box::new(ObjectTy::Boolean)),
            },
        },
        function: |args| {
            let boolean = args.obj.as_boolean().ok_or_else(|| Error::TypeError {
                span: args.method_span.clone(),
                expected: vec![ObjectTy::Boolean],
                found: args.obj.get_type(),
                context: Some(args.call_span.clone()),
            })?;

            Ok(Object::Boolean(!boolean))
        },
    }]);
}