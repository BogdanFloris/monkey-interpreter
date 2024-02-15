use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::object::Object;

#[derive(PartialEq, Debug, Clone)]
pub struct Environment {
    store: HashMap<String, Object>,
    parent: Option<Rc<RefCell<Environment>>>,
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
            parent: None,
        }
    }

    pub fn new_with_parent(parent: Rc<RefCell<Environment>>) -> Self {
        Environment {
            store: HashMap::new(),
            parent: Some(parent),
        }
    }

    pub fn set(&mut self, name: &str, val: Object) {
        self.store.insert(name.to_string(), val);
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        match self.store.get(name) {
            Some(o) => Some(o.clone()),
            None => match self.parent {
                Some(ref parent_env) => parent_env.borrow().get(name),
                None => None,
            },
        }
    }
}
