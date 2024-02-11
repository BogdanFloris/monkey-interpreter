use std::fmt::Display;

pub type Program = Vec<Stmt>;

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Let(Ident, Expr),
    Return(Expr),
    Expr(Expr),
    Block(Vec<Stmt>),
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Let(ident, expr) => write!(f, "let {ident} = {expr};"),
            Stmt::Return(expr) => write!(f, "return {expr};"),
            Stmt::Expr(expr) => write!(f, "{expr};"),
            Stmt::Block(stmts) => {
                writeln!(f, "{{")?;
                for stmt in stmts {
                    writeln!(f, "  {stmt}")?;
                }
                write!(f, "}}")
            }
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Expr {
    Ident(Ident),
    Literal(Literal),
    Prefix(Prefix, Box<Expr>),
    Infix(Box<Expr>, Infix, Box<Expr>),
    If(Box<Expr>, Box<Stmt>, Option<Box<Stmt>>),
    Function(Vec<Ident>, Box<Stmt>),
    Call(Box<Expr>, Vec<Expr>),
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Ident(ident) => write!(f, "{ident}"),
            Expr::Literal(literal) => write!(f, "{literal}"),
            Expr::Prefix(op, expr) => write!(f, "({op}{expr})"),
            Expr::Infix(left, op, right) => write!(f, "({left} {op} {right})"),
            Expr::If(cond, consequence, alternative) => {
                write!(f, "if {cond} {consequence}")?;
                if let Some(alt) = alternative {
                    write!(f, " else {alt}")?;
                }
                Ok(())
            }
            Expr::Function(params, body) => {
                write!(f, "fn(")?;
                for (i, param) in params.iter().enumerate() {
                    write!(f, "{param}")?;
                    if i < params.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ") {body}")
            }
            Expr::Call(function, args) => {
                write!(f, "{function}(")?;
                for arg in args {
                    write!(f, "{arg}")?;
                    if arg != args.last().unwrap() {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")
            }
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Literal {
    Int(i64),
    Bool(bool),
    // StringLiteral(String),
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Int(int) => write!(f, "{int}"),
            Literal::Bool(bool) => write!(f, "{bool}"),
        }
    }
}

#[derive(PartialEq, Debug, Eq, Clone)]
pub struct Ident(pub String);

#[derive(PartialEq, Debug, Clone)]
pub enum Prefix {
    Minus,
    Not,
}

impl Display for Prefix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Prefix::Minus => write!(f, "-"),
            Prefix::Not => write!(f, "!"),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Infix {
    Plus,
    Minus,
    Multiply,
    Divide,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
}

impl Display for Infix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Infix::Plus => write!(f, "+"),
            Infix::Minus => write!(f, "-"),
            Infix::Multiply => write!(f, "*"),
            Infix::Divide => write!(f, "/"),
            Infix::Equal => write!(f, "=="),
            Infix::NotEqual => write!(f, "!="),
            Infix::LessThan => write!(f, "<"),
            Infix::GreaterThan => write!(f, ">"),
        }
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
