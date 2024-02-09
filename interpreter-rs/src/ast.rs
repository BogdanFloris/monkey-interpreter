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
    Prefix(String, Box<Expr>),
    Infix(Box<Expr>, String, Box<Expr>),
    If(Box<Expr>, Box<Stmt>, Option<Box<Stmt>>),
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

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
