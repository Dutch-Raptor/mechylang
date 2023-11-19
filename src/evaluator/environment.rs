use std::{
    collections::HashMap,
    fmt::Debug,
    ops::Deref,
    rc::{Rc, Weak},
    sync::RwLock,
};

use uuid::Uuid;

use super::objects::Object;

#[derive(Debug, Default)]
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

    pub fn get(&self, name: impl Into<Rc<str>>) -> Option<Object> {
        let env = self.inner.read().unwrap();
        env.get(name)
    }

    pub fn set(&mut self, name: impl Into<Rc<str>>, val: Object) {
        let mut env = self.inner.write().unwrap();
        env.set(name, val);
    }

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

    /// Handles dropping an object, decreases the ref count for any objects that are referenced by the object being dropped
    fn handle_drop_obj(&mut self, obj: Object) {
        match obj {
            Object::Array(arr) => {
                for obj in arr {
                    self.handle_drop_obj(obj);
                }
            }
            Object::Reference(reference) => self.handle_drop_for_id(reference.uuid),
            _ => {}
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
struct HeapObject {
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
