pub type Program = Vec<Stmt>;

pub enum Stmt {
    LetStmt(Ident, Expr),
    ReturnStmt(Expr),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Expr {
    IdentExpr(Ident),
    LiteralExpr(Literal),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Literal {
    IntLiteral(i64),
    // BoolLiteral(bool),
    // StringLiteral(String),
}

#[derive(PartialEq, Debug, Eq, Clone)]
pub struct Ident(pub String);
