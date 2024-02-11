use std::fmt::Display;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
    ReturnValue(Box<Object>),
    Error(String),
}

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
