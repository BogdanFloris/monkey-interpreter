use crate::{
    ast::{Expr, Ident, Literal, Program, Stmt},
    lexer::Lexer,
    token::Token,
};

pub struct Parser {
    lexer: Lexer,
    cur_token: Option<Token>,
    peek_token: Option<Token>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut p = Self {
            lexer,
            cur_token: None,
            peek_token: None,
        };
        // Read two tokens, so cur_token and peek_token are both set
        p.next_token();
        p.next_token();
        p
    }

    pub fn next_token(&mut self) {
        self.cur_token = self.peek_token.take();
        self.peek_token = Some(self.lexer.next_token());
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Vec::new();
        while let Some(token) = self.cur_token.clone() {
            if let Token::Eof = token {
                break;
            }
            let stmt = self.parse_statement();
            if let Some(stmt) = stmt {
                program.push(stmt);
            }
            self.next_token();
        }
        program
    }

    fn parse_statement(&mut self) -> Option<Stmt> {
        match self.cur_token.clone() {
            Some(Token::Let) => self.parse_let_statement(),
            _ => None,
        }
    }

    fn parse_let_statement(&mut self) -> Option<Stmt> {
        if let Some(Token::Ident(ident)) = self.peek_token.clone() {
            self.next_token();
            let ident = Ident(ident);
            if let Some(Token::Assign) = self.peek_token.clone() {
                self.next_token();
                let expr = self.parse_expression().unwrap();
                return Some(Stmt::LetStmt(ident, expr));
            }
            return None;
        }
        None
    }

    fn parse_expression(&mut self) -> Option<Expr> {
        self.next_token();
        match self.cur_token.clone() {
            Some(Token::Int(int)) => Some(Expr::LiteralExpr(Literal::IntLiteral(int))),
            Some(Token::Ident(ident)) => Some(Expr::IdentExpr(Ident(ident))),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Expr, Literal, Stmt};

    #[test]
    fn test_let_statement() {
        let input = "
        let x = 5;
        let y = 10;
        let foobar = 838383;
        ";
        let lexer = crate::lexer::Lexer::new(input.to_string());
        let mut parser = crate::parser::Parser::new(lexer);
        let program = parser.parse_program();
        assert_eq!(program.len(), 3);

        let tests = ["x", "y", "foobar"];
        let literals = [5, 10, 838_383];
        for (i, test) in tests.iter().enumerate() {
            let stmt = &program[i];
            match stmt {
                Stmt::LetStmt(ident, expr) => {
                    assert_eq!(ident.0, *test);
                    match expr {
                        Expr::LiteralExpr(literal) => match literal {
                            Literal::IntLiteral(int) => {
                                assert_eq!(*int, literals[i]);
                            }
                        },
                        Expr::IdentExpr(_) => panic!("expected literal expression"),
                    }
                }
            }
        }
    }
}
