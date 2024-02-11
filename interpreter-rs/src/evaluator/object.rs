use std::fmt::Display;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
    ReturnValue(Box<Object>),
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
        }
    }
}
