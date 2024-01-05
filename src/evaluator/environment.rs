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
//! use mechylang::evaluator::environment::Environment;
//! use mechylang::evaluator::objects::Object;
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
//! enclosed_env.update("y", Object::Integer(2));
//! // Now the value of `y` is updated in the outer environment
//! // which the enclosed environment has access to
//! assert_eq!(env.get("y"), Some(Object::Integer(2)));
//! assert_eq!(enclosed_env.get("y"), Some(Object::Integer(2)));
//! ```
//!
//! For more info on [`Environment::set`](Environment::set), [`Environment::update`](Environment::update),
//! [`Environment::get`](Environment::get) and [`Environment::new_enclosed`](Environment::new_enclosed)
//! see their respective documentation.
use std::{
    collections::HashMap,
    fmt::Debug,
    rc::{Rc, Weak},
    sync::RwLock,
};

use uuid::Uuid;

use super::objects::Object;

#[derive(Debug, Default)]
/// An environment is a representation of a scope, which also has access to any outer scopes.
///
/// # Usage
///
/// ```rust
/// use mechylang::evaluator::environment::Environment;
/// use mechylang::evaluator::objects::Object;
///
/// let mut env = Environment::new();
/// env.set("x", Object::Integer(1));
/// assert_eq!(env.get("x"), Some(Object::Integer(1)));
/// ```

pub struct Environment {
    pub inner: Rc<RwLock<InnerEnvironment>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            inner: Rc::new(RwLock::new(InnerEnvironment {
                variables: HashMap::new(),
                heap: Rc::new(RwLock::new(HashMap::new())),
                outer: None,
            })),
        }
    }

    /// Creates a new environment that has access to the variables in the outer environment
    ///
    /// # Example
    ///
    /// ```rust
    /// use mechylang::evaluator::environment::Environment;
    /// use mechylang::evaluator::objects::Object;
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
    /// enclosed_env.update("y", Object::Integer(2));
    /// // Now the value of `y` is updated in the outer environment
    /// // which the enclosed environment has access to
    /// assert_eq!(env.get("y"), Some(Object::Integer(2)));
    /// assert_eq!(enclosed_env.get("y"), Some(Object::Integer(2)));
    /// ```
    ///
    /// For more info on [Environment::set](Environment::set), [Environment::update](Environment::update)
    /// and [Environment::get](Environment::get) see their respective documentation
    pub fn new_enclosed(outer: &Environment) -> Self {
        Self {
            inner: Rc::new(RwLock::new(InnerEnvironment {
                variables: HashMap::new(),
                heap: outer.inner.read().unwrap().heap.clone(),
                outer: Some(Rc::downgrade(&outer.inner)),
            })),
        }
    }

    pub fn set_outer(&mut self, outer: &Environment) {
        let mut env = self.inner.write().unwrap();
        env.outer = Some(Rc::downgrade(&outer.inner));
    }

    pub fn get_all_keys(&self) -> Vec<Rc<str>> {
        let env = self.inner.read().unwrap();
        let mut keys = env.variables.keys().cloned().collect::<Vec<_>>();
        dbg!(&env.outer);
        if let Some(outer) = &env.outer {
            keys.extend(Self::get_all_keys_from_outer(outer));
        }
        keys
    }

    fn get_all_keys_from_outer(outer: &Weak<RwLock<InnerEnvironment>>) -> Vec<Rc<str>> {
        let outer_ref = outer.upgrade().unwrap();
        let outer_env = outer_ref.read().unwrap();
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
    /// use mechylang::evaluator::environment::Environment;
    /// use mechylang::evaluator::objects::Object;
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
        let env = self.inner.read().unwrap();
        env.get(name)
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
    /// use mechylang::evaluator::environment::Environment;
    /// use mechylang::evaluator::objects::Object;
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
        let mut env = self.inner.write().unwrap();
        env.set(name, val);
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
    /// use mechylang::evaluator::environment::Environment;
    /// use mechylang::evaluator::objects::Object;
    ///
    /// let mut env = Environment::new();
    /// env.set("x", Object::Integer(1));
    /// assert_eq!(env.get("x"), Some(Object::Integer(1)));
    /// env.update("x", Object::Integer(2));
    /// assert_eq!(env.get("x"), Some(Object::Integer(2)));
    ///
    /// let mut enclosed_env = Environment::new_enclosed(&env);
    /// // The enclosed env also has access to the value
    /// assert_eq!(enclosed_env.get("x"), Some(Object::Integer(2)));
    ///
    /// enclosed_env.update("x", Object::Integer(5));
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
        let mut env = self.inner.write().unwrap();
        env.update(name, val)
    }

    pub fn mutate(
        &mut self,
        name: impl Into<Rc<str>>,
        func: impl FnOnce(&mut Object) -> Result<Object, String>,
    ) -> Result<Object, String> {
        let mut env = self.inner.write().unwrap();
        env.mutate(name, func)
    }

    pub fn delete(&mut self, name: impl Into<Rc<str>>) -> Option<Object> {
        let mut env = self.inner.write().unwrap();
        env.delete(name)
    }
}

#[derive(Default)]
pub struct InnerEnvironment {
    pub variables: HashMap<Rc<str>, Uuid>,
    pub heap: Rc<RwLock<HashMap<Uuid, HeapObject>>>,
    pub outer: Option<Weak<RwLock<InnerEnvironment>>>,
}

impl InnerEnvironment {
    fn set_object(&mut self, name: Rc<str>, obj: Object) -> Uuid {
        let id = self.generate_id();
        let mut heap = self.heap.write().unwrap();
        heap.insert(id, HeapObject { ref_count: 1, obj });
        self.variables.insert(name, id);
        id
    }

    /// Generates a unique id for a new object
    fn generate_id(&mut self) -> Uuid {
        let mut id = Uuid::new_v4();
        let heap = self.heap.read().unwrap();
        while heap.contains_key(&id) {
            id = Uuid::new_v4();
        }
        id
    }

    /// Handles dropping an object by id, decreases the ref count for any objects that are referenced by the object being dropped
    fn handle_drop_for_id(&mut self, id: Uuid) {
        let mut heap = self.heap.write().unwrap();
        if let Some(HeapObject { ref_count, .. }) = heap.get_mut(&id) {
            *ref_count -= 1;
            if *ref_count == 0 {
                heap.remove(&id);
            }
        }
    }

    /// Recursively get a value from the environment
    ///
    /// Keeps trying to get the value from the outer environment until it succeeds or there are no more outer
    pub fn get(&self, name: impl Into<Rc<str>>) -> Option<Object> {
        let name = name.into();
        match self.variables.get(&name) {
            Some(id) => {
                let heap = self.heap.read().unwrap();
                let heap_obj = heap.get(id)?;
                Some(heap_obj.obj.clone())
            }
            None => match &self.outer {
                Some(outer) => {
                    let outer_ref = outer.upgrade()?;
                    let outer_env = outer_ref.read().unwrap();
                    outer_env.get(name)
                }
                None => None,
            },
        }
    }

    /// Set a value in the current scope
    ///
    /// If a value with the same name already exists, in the current scope
    /// it will be overwritten, if no other reference to the old value exist
    /// it will be dropped
    pub fn set(&mut self, name: impl Into<Rc<str>>, val: Object) {
        let name = name.into();

        if let Some(old_id) = self.variables.get(&name) {
            self.handle_drop_for_id(*old_id);
        }

        self.set_object(name, val);
    }

    pub fn update(&mut self, name: impl Into<Rc<str>>, val: Object) -> Result<(), String> {
        let name = name.into();
        match self.variables.get_mut(&name) {
            Some(id) => {
                let mut heap = self.heap.write().unwrap();
                let heap_obj = heap.get_mut(id).unwrap();
                heap_obj.obj = val;
            }
            None => {
                return match &self.outer {
                    Some(outer) => {
                        let outer_ref = match outer.upgrade() {
                            Some(outer) => outer,
                            None => {
                                return Err("Cannot mutate variable that does not exist".to_string())
                            }
                        };
                        let mut outer_env = outer_ref.write().unwrap();
                        outer_env.update(name, val)
                    }
                    None => Err("Cannot mutate variable that does not exist".to_string()),
                }
            }
        };

        Ok(())
    }

    /// A function to update a value in the environment recursively
    ///
    /// This function takes a name and a function that takes a mutable reference to an Object
    /// It then searches for the value in the environment and applies the function to it
    pub fn mutate(
        &mut self,
        name: impl Into<Rc<str>>,
        func: impl FnOnce(&mut Object) -> Result<Object, String>,
    ) -> Result<Object, String> {
        let name = name.into();
        if let Some(id) = self.variables.get_mut(&name) {
            if let Some(HeapObject { ref mut obj, .. }) = self.heap.write().unwrap().get_mut(id) {
                func(obj)
            }
            // If the object is not in the heap, return an error
            else {
                Err("Cannot mutate variable that does not exist".to_string())
            }
        } else if let Some(outer) = &self.outer {
            match outer.upgrade() {
                Some(outer) => outer.write().unwrap().mutate(name, func),
                None => Err("Cannot mutate variable that does not exist".to_string()),
            }
        } else {
            Err("Cannot mutate variable that does not exist".to_string())
        }
    }

    /// Delete a value from the environment
    ///
    /// If the value is not in the current scope, it will try to delete it from the outer scope
    /// If the value is not in the outer scope, it will return None
    pub fn delete(&mut self, name: impl Into<Rc<str>>) -> Option<Object> {
        let name = name.into();
        if let Some(id) = self.variables.remove(&name) {
            let mut heap = self.heap.write().unwrap();
            let heap_obj = heap.remove(&id)?;
            Some(heap_obj.obj)
        } else if let Some(outer) = &self.outer {
            let outer_ref = outer.upgrade()?;
            let mut outer_env = outer_ref.write().unwrap();
            outer_env.delete(name)
        } else {
            None
        }
    }
}

impl Drop for InnerEnvironment {
    fn drop(&mut self) {
        for uuid in self.variables.clone().values() {
            self.handle_drop_for_id(*uuid);
        }
    }
}

pub struct HeapObject {
    ref_count: usize,
    obj: Object,
}

impl Debug for InnerEnvironment {
    /// Debug implementation for InnerEnvironment
    ///
    /// Prints the store and whether or not there is an outer environment
    /// does not print the outer environment, as that would cause an infinite loop
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let has_outer = self.outer.is_some();
        write!(
            f,
            "InnerEnvironment {{ store: {{ {:?} }}, has_outer: {} }}",
            self.variables, has_outer
        )
    }
}

impl Clone for Environment {
    fn clone(&self) -> Self {
        Self {
            inner: Rc::clone(&self.inner),
        }
    }
}
