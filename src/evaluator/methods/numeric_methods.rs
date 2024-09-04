use std::sync::Arc;
use lazy_static::lazy_static;
use crate::evaluator::Error;
use crate::evaluator::objects::{ArgumentList, ArgumentType, FunctionTy, MethodTy, ObjectTy};
use crate::Object;

use super::MethodInner;

lazy_static! {
    /// ## `pow(Integer) -> Integer`
    /// Raises the integer to the power of the given integer
    /// ```rust
    /// # mechylang::test_utils::test_eval_ok(r#"
    /// assert_eq(2.pow(3), 8);
    /// assert_eq(2.pow(0), 1);
    /// # "#);
    /// ```
    pub static ref FLOAT_METHODS: Arc<[MethodInner]> = Arc::new([
        MethodInner {
            method_ty: MethodTy {
                method_name: "pow",
                self_ty: Box::new(ObjectTy::Float),
                function_ty: FunctionTy {
                    arguments: ArgumentList::new_exactly(vec![ArgumentType { name: "exponent".into(), ty: ObjectTy::Integer }]),
                    expected_return_type: Some(Box::new(ObjectTy::Integer)),
                },
            },
            function: |args| {
                let base = args.obj.as_float().expect("Expected obj to be an integer");
                let exponent = args.args[0].as_integer().ok_or_else(|| Error::TypeError {
                    span: args.method_span.clone(),
                    expected: vec![ObjectTy::Integer],
                    found: args.args[0].get_type(),
                })?;

                Ok(Object::Float(base.powi(exponent as i32)))
            },
        },
        MethodInner {
            method_ty: MethodTy {
                method_name: "abs",
                self_ty: Box::new(ObjectTy::Float),
                function_ty: FunctionTy {
                    arguments: ArgumentList::None,
                    expected_return_type: Some(Box::new(ObjectTy::Float)),
                },
            },
            function: |args| {
                let float = args.obj.as_float().ok_or_else(|| Error::TypeError {
                    span: args.method_span.clone(),
                    expected: vec![ObjectTy::Float],
                    found: args.obj.get_type(),
                })?;

                Ok(Object::Float(float.abs()))
            }
        },
    ]);
}
lazy_static! {
    /// Methods for the `Object::Integer` type
    ///
    /// # Integer Methods
    ///
    /// ## Pow
    /// `pow(Integer) -> Integer`
    /// Raises the integer to the power of the given integer
    ///
    /// ```rust
    /// # mechylang::test_utils::test_eval_ok(r#"
    /// assert_eq(2.pow(3), 8);
    /// assert_eq(2.pow(0), 1);
    /// # "#);
    /// ```
    pub static ref INTEGER_METHODS: Arc<[MethodInner]> = Arc::new([MethodInner {
        method_ty: MethodTy {
            method_name: "pow",
            self_ty: Box::new(ObjectTy::Integer),
            function_ty: FunctionTy {
                arguments: ArgumentList::new_exactly(vec![ArgumentType { name: "exponent".into(), ty: ObjectTy::Integer }]),
                expected_return_type: Some(Box::new(ObjectTy::Integer)),
            },
        },
        function: |args| {
            debug_assert!(matches!(args.obj, Object::Integer(_)), "Expected obj to be an integer");

            let base = args.obj.as_integer().expect("Expected obj to be an integer");
            let exponent = args.args[0].as_integer().ok_or_else(|| Error::TypeError {
                span: args.method_span.clone(),
                expected: vec![ObjectTy::Integer],
                found: args.args[0].get_type(),
            })?;

            Ok(Object::Integer(base.pow(exponent as u32)))
        },
    },
        MethodInner {
            method_ty: MethodTy {
                method_name: "abs",
                self_ty: Box::new(ObjectTy::Integer),
                function_ty: FunctionTy {
                    arguments: ArgumentList::None,
                    expected_return_type: Some(Box::new(ObjectTy::Integer)),
                },
            },
            function: |args| {
                let integer = args.obj.as_integer().expect("Expected obj to be an integer");

                Ok(Object::Integer(integer.abs()))
            }
        },
        
    ]);
}

