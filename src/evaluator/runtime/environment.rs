//! # Environment
//!
//! The environment is a data structure that stores variables and their values for `mechylang`
//! programs. The environment is a tree of scopes, where each scope has access to the variables
//! in connected outer scopes. The environment is used to store variables and their values
//! during the evaluation of a `mechylang` program.
//!
//! ## Scope
//! A scope is a mapping of variable names to their values. A scope can be thought of as a
//! dictionary or a hashmap. A scope can be empty, in which case it has no variables and
//! no values. A scope can also be non-empty, in which case it has variables and values.
//! A scope can also be nested inside another scope. A scope that is nested inside another
//! scope is called an enclosed scope. An enclosed scope has access to the variables and
//! values in the outer scope. A scope that is not nested inside another scope is called
//! a global scope. A global scope has no access to any other scope.
//!
//! ```text
//! Global Scope {
//!    x: 1, y: 2, z: 3
//!    // has access to x, y, z
//!    Enclosed Scope {
//!         foo: 4, bar: 5
//!         // has access to x, y, z and foo, bar
//!         Enclosed Scope {
//!             baz: 6
//!             // has access to x, y, z, foo, bar and baz
//!         }
//!         
//!         Enclosed Scope {
//!             qux: 7
//!             // has access to x, y, z, foo, bar and qux
//!         }
//!    }
//!    Enclosed Scope {
//!         quux: 8
//!         // has access to x, y, z and quux
//!    }
//! }
//! ```
//!
//! ## Environment
//! An environment is a representation of a scope, which also has access to any outer scopes.
//!
//! `mechylang uses one global heap to store all objects. Variables are stored per scope.
//! This means that multiple scopes can have access to the same object on the heap, but
//! each scope can only access the variables it has access to.
//!
//!
//!
//! ### Shadowing
//! A scope can have a variable with the same name as a variable in an outer scope. In this
//! case, the variable in the inner scope shadows the variable in the outer scope. This means
//! that the variable in the outer scope is not accessible from the inner scope. Only the
//! variable in the inner scope is accessible from the inner scope. This is called shadowing.
//!
//! ```rust
//! # mechylang::test_utils::test_eval_ok(r#"
//! let x = 1;
//! assert_eq(x, 1);
//! { // This is a new scope
//!     assert_eq(x, 1); // x is accessible from this scope
//!     let x = 2;
//!     assert_eq(x, 2);
//! } // The scope ends here
//! // In the outer scope, x is still 1
//! assert_eq(x, 1);
//! # "#);
//! ```
//!
//! How does this work? When a variable is set in a scope, it is simply added to, or overwritten
//! in the current scope. Any potential outer scopes are ignored.
//!
//!
//! ### Updating
//! A scope can also update a variable in an outer scope. This means that the variable in the
//! outer scope is updated to the new value. This is called updating.
//!
//! ```rust
//! # mechylang::test_utils::test_eval_ok(r#"
//! let x = 1;
//! assert_eq(x, 1);
//! { // This is a new scope
//!     assert_eq(x, 1); // x is accessible from this scope
//!     x = 2;
//!     assert_eq(x, 2);
//!     { // This is a new scope
//!         assert_eq(x, 2); // x is accessible from this scope
//!         x = 3;
//!         assert_eq(x, 3);
//!     } // The scope ends here
//!     // In the outer scope, x is now 3
//!     assert_eq(x, 3);
//! } // The scope ends here
//! // In the outer scope, x is now 3
//! assert_eq(x, 3);
//! # "#);
//! ```
//!
//! How does this work? When a variable is updated in a scope, the variable is first searched
//! for in the current scope. If the variable is found in the current scope, then the variable
//! is updated in the current scope. If the variable is not found in the current scope, then
//! the variable is searched for in the outer scopes recursively. The value is updated in the
//! first scope where the variable is found. If the variable is not found in any scope, then
//! an error is returned.
//!
//! For more info on updating see [`Environment::update`](Environment::update)
//!
//! # Example
//!
//! ```rust
//! use mechylang::Environment;
//! use mechylang::Object;
//!
//! let mut env = Environment::new();
//! env.set("x", Object::Integer(1));
//! assert_eq!(env.get("x"), Some(Object::Integer(1)));
//!
//! // The enclosed env also has access to the value
//! let mut enclosed_env = Environment::new_enclosed(&env);
//! assert_eq!(enclosed_env.get("x"), Some(Object::Integer(1)));
//!
//! // The value can be set in the enclosed environment
//! enclosed_env.set("x", Object::Integer(2));
//! // Now this new value for x shadows the old value for x
//! assert_eq!(enclosed_env.get("x"), Some(Object::Integer(2)));
//! assert_eq!(env.get("x"), Some(Object::Integer(1)));
//!
//! // If we want to update the first value found (by searching outwards from the current scope)
//! // we can use Environment::update
//! env.set("y", Object::Integer(1)); // Set a new variable as `x` is currently shadowed
//! assert_eq!(env.get("y"), Some(Object::Integer(1)));
//! assert_eq!(enclosed_env.get("y"), Some(Object::Integer(1)));
//!
//! // Update the value of `y` in the enclosed environment
//! enclosed_env.update("y", Object::Integer(2)).unwrap();
//! // Now the value of `y` is updated in the outer environment
//! // which the enclosed environment has access to
//! assert_eq!(env.get("y"), Some(Object::Integer(2)));
//! assert_eq!(enclosed_env.get("y"), Some(Object::Integer(2)));
//! ```
//!
//! For more info on [`Environment::set`](Environment::set), [`Environment::update`](Environment::update),
//! [`Environment::get`](Environment::get) and [`Environment::new_enclosed`](Environment::new_enclosed)
//! see their respective documentation.
use crate::Object;
use std::{
    collections::HashMap,
    fmt::Debug,
    rc::Rc,
};
use std::fmt::{Display, Formatter};
use std::sync::Mutex;
use crate::evaluator::runtime::inner_environment::InnerEnvironment;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ObjectId(u32);

impl Display for ObjectId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl ObjectId {
    pub fn random() -> Self {
        Self(rand::random::<u32>())
    }
}


/// An environment is a representation of a scope, which also has access to any outer scopes.
///
/// # Usage
///
/// ```rust
/// use mechylang::Environment;
/// use mechylang::Object;
///
/// let mut env = Environment::new();
/// env.set("x", Object::Integer(1));
/// assert_eq!(env.get("x"), Some(Object::Integer(1)));
/// ```
#[derive(Debug, Default)]
pub struct Environment {
    pub inner: Rc<Mutex<InnerEnvironment>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            inner: Rc::new(Mutex::new(InnerEnvironment {
                variables: HashMap::new(),
                heap: Rc::new(Mutex::new(HashMap::new())),
                outer: None,
            })),
        }
    }

    /// Creates a new environment that has access to the variables in the outer environment
    ///
    /// # Example
    ///
    /// ```rust
    /// use mechylang::Environment;
    /// use mechylang::Object;
    ///
    /// let mut env = Environment::new();
    /// env.set("x", Object::Integer(1));
    /// assert_eq!(env.get("x"), Some(Object::Integer(1)));
    ///
    /// // The enclosed env also has access to the value
    /// let mut enclosed_env = Environment::new_enclosed(&env);
    /// assert_eq!(enclosed_env.get("x"), Some(Object::Integer(1)));
    ///
    /// // The value can be set in the enclosed environment
    /// enclosed_env.set("x", Object::Integer(2));
    /// // Now this new value for x shadows the old value for x
    /// assert_eq!(enclosed_env.get("x"), Some(Object::Integer(2)));
    /// assert_eq!(env.get("x"), Some(Object::Integer(1)));
    ///
    /// // If we want to update the first value found (by searching outwards from the current scope)
    /// // we can use Environment::update
    /// env.set("y", Object::Integer(1)); // Set a new variable as `x` is currently shadowed
    /// assert_eq!(env.get("y"), Some(Object::Integer(1)));
    /// assert_eq!(enclosed_env.get("y"), Some(Object::Integer(1)));
    ///
    /// // Update the value of `y` in the enclosed environment
    /// enclosed_env.update("y", Object::Integer(2)).unwrap();
    /// // Now the value of `y` is updated in the outer environment
    /// // which the enclosed environment has access to
    /// assert_eq!(env.get("y"), Some(Object::Integer(2)));
    /// assert_eq!(enclosed_env.get("y"), Some(Object::Integer(2)));
    /// ```
    ///
    /// For more info on [Environment::set](Environment::set), [Environment::update](Environment::update)
    /// and [Environment::get](Environment::get) see their respective documentation
    pub fn new_enclosed(outer_env: &Environment) -> Self {
        let outer_inner = Rc::clone(&outer_env.inner);
        let heap = outer_inner.lock().unwrap().heap.clone();
        Self {
            inner: Rc::new(Mutex::new(InnerEnvironment {
                variables: HashMap::new(),
                heap,
                outer: Some(outer_inner),
            })),
        }
    }

    pub fn get_all_keys(&self) -> Vec<Rc<str>> {
        let env = self.inner.lock().unwrap();
        let mut keys = env.variables.keys().cloned().collect::<Vec<_>>();
        dbg!(&env.outer);
        if let Some(outer) = &env.outer {
            keys.extend(Self::get_all_keys_from_outer(outer));
        }
        keys
    }

    fn get_all_keys_from_outer(outer: &Mutex<InnerEnvironment>) -> Vec<Rc<str>> {
        let outer_env = outer.lock().unwrap();
        let mut keys = outer_env.variables.keys().cloned().collect::<Vec<_>>();
        if let Some(outer) = &outer_env.outer {
            keys.extend(Self::get_all_keys_from_outer(outer));
        }
        keys
    }

    /// Gets a value from the environment recursively
    ///
    /// This function takes a name and searches for the value in the environment
    /// If the value is not found it is searched for in the outer environments
    /// If the value is not found in any of the outer environments, None is returned
    /// If a value is found, it is returned
    ///
    /// # Example
    ///
    /// ```rust
    /// use mechylang::Environment;
    /// use mechylang::Object;
    ///
    /// let mut env = Environment::new();
    ///
    /// // The environment is empty so the value is not found
    /// assert_eq!(env.get("x"), None);
    ///
    /// // The value can be set in the current scope
    /// env.set("x", Object::Integer(1));
    ///
    /// // The value can be retrieved from the current scope
    /// assert_eq!(env.get("x"), Some(Object::Integer(1)));
    ///
    /// // The value can be retrieved from an outer scope
    /// let mut enclosed_env = Environment::new_enclosed(&env);
    /// assert_eq!(enclosed_env.get("x"), Some(Object::Integer(1)));
    ///
    /// // Setting the value in the enclosed environment will not affect the outer environment
    /// enclosed_env.set("x", Object::Integer(2));
    /// assert_eq!(enclosed_env.get("x"), Some(Object::Integer(2)));
    /// assert_eq!(env.get("x"), Some(Object::Integer(1)));
    /// ```
    ///
    /// For more info on [Environment::set](Environment::set), [Environment::new_enclosed](Environment::new_enclosed)
    /// see their respective documentation
    pub fn get(&self, name: impl Into<Rc<str>>) -> Option<Object> {
        let env = self.inner.lock().expect("environment to be able to get");
        env.get(name)
    }
    
    pub fn get_by_id(&self, id: ObjectId) -> Option<Object> {
        let env = self.inner.lock().expect("environment to be able to get");
        env.get_by_id(id)
    }

    pub fn get_id(&self, name: impl Into<Rc<str>>) -> Option<ObjectId> {
        let env = self.inner.lock().expect("environment to be able to get");
        env.get_id(name)
    }

    /// Sets a value in the current scope
    ///
    /// If a value with the same name already exists, in the current scope
    /// it will be overwritten, and if no other reference to the old value exists
    /// the old value will be dropped
    ///
    /// # Example
    ///
    /// ```rust
    /// use mechylang::Environment;
    /// use mechylang::Object;
    ///
    /// let mut env = Environment::new();
    /// env.set("x", Object::Integer(1));
    /// assert_eq!(env.get("x"), Some(Object::Integer(1)));
    ///
    /// // The value can be overwritten
    /// env.set("x", Object::Integer(2));
    /// assert_eq!(env.get("x"), Some(Object::Integer(2)));
    /// ```
    /// For more info on [Environment::get](Environment::get) see its documentation
    pub fn set(&mut self, name: impl Into<Rc<str>>, val: Object) {
        let mut env = self.inner.lock().expect("environment to be able to set");
        env.set(name, val);
    }
    
    pub fn set_by_id(&mut self, id: ObjectId, val: Object) {
        let mut env = self.inner.lock().expect("environment to be able to set");
        env.set_by_id(id, val);
    }

    /// Update a value in the environment recursively
    ///
    /// This function takes a name and a value and searches for the value in the environment
    /// If the value is found it is updated, if not it is searched for recursively in the outer environments
    /// and updated in the first environment where it is found.
    /// If the value is not found in any of the outer environments, an error is returned
    ///
    /// # Example
    /// ```
    /// use mechylang::Environment;
    /// use mechylang::Object;
    ///
    /// let mut env = Environment::new();
    /// env.set("x", Object::Integer(1));
    /// assert_eq!(env.get("x"), Some(Object::Integer(1)));
    /// env.update("x", Object::Integer(2)).unwrap();
    /// assert_eq!(env.get("x"), Some(Object::Integer(2)));
    ///
    /// let mut enclosed_env = Environment::new_enclosed(&env);
    /// // The enclosed env also has access to the value
    /// assert_eq!(enclosed_env.get("x"), Some(Object::Integer(2)));
    ///
    /// enclosed_env.update("x", Object::Integer(5)).unwrap();
    /// // The value is only updated in the outer environment, but
    /// // Environment::get will search for the value in the outer environment
    /// // so the new value is returned
    /// assert_eq!(enclosed_env.get("x"), Some(Object::Integer(5)));
    /// // Of course getting the value from the outer environment will also return the new value
    /// assert_eq!(env.get("x"), Some(Object::Integer(5)));
    /// ```
    ///
    /// For more info on [Environment::set](Environment::set) and [Environment::get](Environment::get)
    /// see their respective documentation
    pub fn update(&mut self, name: impl Into<Rc<str>>, val: Object) -> Result<(), String> {
        let mut env = self.inner.lock().expect("environment to be able to update");
        env.update(name, val)
    }

    pub fn mutate(
        &mut self,
        name: impl Into<Rc<str>>,
        func: impl FnOnce(&mut Object) -> Result<Object, String>,
    ) -> Result<Object, String> {
        let mut env = self.inner.lock().expect("environment to be able to mutate");
        env.mutate(name, func)
    }

    pub fn delete(&mut self, name: impl Into<Rc<str>>) -> Option<Object> {
        let mut env = self.inner.lock().expect("environment to be able to delete");
        env.delete(name)
    }

    pub fn store_object(&self, object: Object) -> ObjectId {
        self.inner.lock().unwrap().store_object(object)
    }
}


impl Clone for Environment {
    fn clone(&self) -> Self {
        Self {
            inner: Rc::clone(&self.inner),
        }
    }
}
