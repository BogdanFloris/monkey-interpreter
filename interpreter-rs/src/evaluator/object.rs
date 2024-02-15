use std::{cell::RefCell, fmt::Display, rc::Rc};

use crate::parser::ast::{Ident, Program};

use super::enviroment::Environment;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
    ReturnValue(Box<Object>),
    Function(Vec<Ident>, Program, Rc<RefCell<Environment>>),
    Error(String),
}

impl Eq for Object {}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Object::Integer(ref value) => write!(f, "{value}"),
            Object::Boolean(ref value) => {
                if *value {
                    write!(f, "true")
                } else {
                    write!(f, "false")
                }
            }
            Object::Null => write!(f, "null"),
            Object::ReturnValue(ref value) => write!(f, "{value}"),
            Object::Function(ref params, ref body, _) => {
                let params_str = params
                    .iter()
                    .map(std::string::ToString::to_string)
                    .collect::<Vec<String>>()
                    .join(", ");
                let mut body_str = String::new();
                for stmt in body {
                    body_str.push_str("  ");
                    body_str.push_str(&stmt.to_string());
                }
                write!(f, "fn({params_str}) {{\n{body_str}\n}}")
            }
            Object::Error(ref msg) => write!(f, "ERROR: {msg}"),
        }
    }
}

impl Object {
    pub fn is_truthy(&self) -> bool {
        match *self {
            Object::Boolean(value) => value,
            Object::Null => false,
            _ => true,
        }
    }
}
