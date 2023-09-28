use std::{
    collections::HashMap,
    fmt::Debug,
    rc::{Rc, Weak},
    sync::RwLock,
};

use super::objects::Object;

#[derive(Debug, Default)]
pub struct Environment {
    inner: Rc<RwLock<InnerEnvironment>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            inner: Rc::new(RwLock::new(InnerEnvironment {
                store: HashMap::new(),
                outer: None,
            })),
        }
    }

    pub fn new_enclosed(outer: &Environment) -> Self {
        Self {
            inner: Rc::new(RwLock::new(InnerEnvironment {
                store: HashMap::new(),
                outer: Some(Rc::downgrade(&outer.inner)),
            })),
        }
    }

    pub fn set_outer(&mut self, outer: &Environment) {
        let mut env = self.inner.write().unwrap();
        env.outer = Some(Rc::downgrade(&outer.inner));
    }

    /// Recursively get a value from the environment
    ///
    /// Keeps trying to get the value from the outer environment until it succeeds or there are no more outer
    pub fn get(&self, name: impl Into<Rc<str>>) -> Option<Object> {
        let name = name.into();
        let env = self.inner.read().unwrap();
        match env.store.get(&name) {
            Some(obj) => Some(obj.clone()),
            None => match &env.outer {
                Some(outer) => Self::get_from_outer(outer, name),
                None => None,
            },
        }
    }

    pub fn set(&mut self, name: impl Into<Rc<str>>, val: Object) {
        let name = name.into();
        let mut env = self.inner.write().unwrap();
        env.store.insert(name, val);
    }

    pub fn mutate(&mut self, name: impl Into<Rc<str>>, val: Object) -> Result<(), String> {
        let name = name.into();
        let mut env = self.inner.write().unwrap();
        match env.store.get_mut(&name) {
            Some(obj) => *obj = val,
            None => {
                return match &env.outer {
                    Some(outer) => Self::mutate_outer(outer, name, val),
                    None => Err("Cannot mutate variable that does not exist".to_string()),
                }
            }
        };

        Ok(())
    }

    /// Recursively get a value from an outer environment
    ///
    /// Keeps trying to upgrade the weak reference until it succeeds or there are no more outer
    fn get_from_outer(
        outer: &Weak<RwLock<InnerEnvironment>>,
        name: impl Into<Rc<str>>,
    ) -> Option<Object> {
        let name = name.into();
        // Try to upgrade the weak reference
        let outer_ref = outer.upgrade()?;

        // Lock the outer environment
        let outer_env = match outer_ref.read() {
            Ok(env) => env,
            Err(_) => return None,
        };

        // Get the value from the outer environment
        match outer_env.store.get(&name) {
            Some(obj) => Some(obj.clone()),
            None => match &outer_env.outer {
                Some(outer) => Self::get_from_outer(outer, name),
                None => None,
            },
        }
    }

    fn mutate_outer(
        outer: &Weak<RwLock<InnerEnvironment>>,
        name: impl Into<Rc<str>>,
        val: Object,
    ) -> Result<(), String> {
        let name = name.into();
        // Try to upgrade the weak reference
        let outer_ref = outer.upgrade().unwrap();

        // Lock the outer environment
        let mut outer_env = outer_ref.write().unwrap();

        // Get the value from the outer environment
        if let Some(obj) = outer_env.store.get_mut(&name) {
            *obj = val;
            return Ok(());
        }

        match &outer_env.outer {
            Some(outer) => Self::mutate_outer(outer, name, val),
            None => Err("Cannot mutate variable that does not exist".to_string()),
        }
    }

    /// A function to update a value in the environment
    ///
    /// This function takes a name and a function that takes a mutable reference to an Object
    /// It then searches for the value in the environment and applies the function to it
    pub fn update(
        &mut self,
        name: impl Into<Rc<str>>,
        func: impl FnOnce(&mut Object) -> Result<Object, String>,
    ) -> Result<Object, String> {
        let name = name.into();
        let mut env = self.inner.write().unwrap();
        if let Some(obj) = env.store.get_mut(&name) {
            func(obj)
        } else if let Some(outer) = &env.outer {
            Self::update_outer(outer, name, func)
        } else {
            Err("Cannot mutate variable that does not exist".to_string())
        }
    }

    /// A function to update a value in an outer environment
    fn update_outer(
        outer: &Weak<RwLock<InnerEnvironment>>,
        name: impl Into<Rc<str>>,
        func: impl FnOnce(&mut Object) -> Result<Object, String>,
    ) -> Result<Object, String> {
        let name = name.into();
        // Try to upgrade the weak reference
        let outer_ref = outer.upgrade().unwrap();

        // Lock the outer environment
        let mut outer_env = outer_ref.write().unwrap();

        // Get the value from the outer environment
        if let Some(obj) = outer_env.store.get_mut(&name) {
            func(obj)
        } else if let Some(outer) = &outer_env.outer {
            Self::update_outer(outer, name, func)
        } else {
            Err("Cannot mutate variable that does not exist".to_string())
        }
    }
}

#[derive(Default)]
struct InnerEnvironment {
    store: HashMap<Rc<str>, Object>,
    outer: Option<Weak<RwLock<InnerEnvironment>>>,
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
            self.store, has_outer
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
