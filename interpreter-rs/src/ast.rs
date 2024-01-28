use std::fmt::Display;

pub type Program = Vec<Stmt>;

pub enum Stmt {
    LetStmt(Ident, Expr),
    ReturnStmt(Expr),
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::LetStmt(ident, expr) => write!(f, "let {ident} = {expr};"),
            Stmt::ReturnStmt(expr) => write!(f, "return {expr};"),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Expr {
    IdentExpr(Ident),
    LiteralExpr(Literal),
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::IdentExpr(ident) => write!(f, "{ident}"),
            Expr::LiteralExpr(literal) => write!(f, "{literal}"),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Literal {
    IntLiteral(i64),
    // BoolLiteral(bool),
    // StringLiteral(String),
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::IntLiteral(int) => write!(f, "{int}"),
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
