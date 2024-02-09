use crate::{
    ast::{Expr, Ident, Literal, Program, Stmt},
    lexer::Lexer,
    token::Token,
};

#[derive(PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // myFunction(X)
}

pub struct Parser {
    lexer: Lexer,
    cur_token: Option<Token>,
    peek_token: Option<Token>,
    pub errors: Vec<String>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut p = Self {
            lexer,
            errors: Vec::new(),
            cur_token: None,
            peek_token: None,
        };

        // Read two tokens, so cur_token and peek_token are both set
        p.next_token();
        p.next_token();
        p
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Vec::new();
        while let Some(token) = self.cur_token.clone() {
            if let Token::Eof = token {
                break;
            }
            let stmt = self.parse_statement();
            if let Some(stmt) = stmt {
                if !(self.peek_token.clone().unwrap() == Token::SemiColon) {
                    self.errors
                        .push(format!("expected semicolon after statement: {stmt}"));
                }
                program.push(stmt);
            }
            self.next_token();
            self.next_token();
        }
        program
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.take();
        self.peek_token = Some(self.lexer.next_token());
    }

    fn peek_error(&mut self, token: &Token) {
        let tok = self.peek_token.clone().unwrap();
        let msg = format!(
            "expected next token to be {:?}, got {:?} instead",
            *token, tok
        );
        self.errors.push(msg);
    }

    fn cur_precedence(&self) -> Precedence {
        match self.cur_token.clone() {
            Some(Token::Eq | Token::NotEq) => Precedence::Equals,
            Some(Token::LessThan | Token::GreaterThan) => Precedence::LessGreater,
            Some(Token::Plus | Token::Minus) => Precedence::Sum,
            Some(Token::Slash | Token::Asterisk) => Precedence::Product,
            Some(Token::LParen) => Precedence::Call,
            _ => Precedence::Lowest,
        }
    }

    fn peek_precedence(&self) -> Precedence {
        match self.peek_token.clone() {
            Some(Token::Eq | Token::NotEq) => Precedence::Equals,
            Some(Token::LessThan | Token::GreaterThan) => Precedence::LessGreater,
            Some(Token::Plus | Token::Minus) => Precedence::Sum,
            Some(Token::Slash | Token::Asterisk) => Precedence::Product,
            Some(Token::LParen) => Precedence::Call,
            _ => Precedence::Lowest,
        }
    }

    fn parse_statement(&mut self) -> Option<Stmt> {
        match self.cur_token.clone() {
            Some(Token::Let) => self.parse_let_statement(),
            Some(Token::Return) => Some(self.parse_return_statement()),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Option<Stmt> {
        if let Some(Token::Ident(ident)) = self.peek_token.clone() {
            self.next_token();
            let ident = Ident(ident);
            if let Some(Token::Assign) = self.peek_token.clone() {
                // skip = token
                self.next_token();
                self.next_token();
                let expr = self.parse_expression(&Precedence::Lowest).unwrap();
                return Some(Stmt::Let(ident, expr));
            }
            self.peek_error(&Token::Assign);
            return None;
        }
        self.peek_error(&Token::Ident(String::new()));
        None
    }

    fn parse_return_statement(&mut self) -> Stmt {
        self.next_token();
        let expr = self.parse_expression(&Precedence::Lowest).unwrap();
        Stmt::Return(expr)
    }

    fn parse_expression_statement(&mut self) -> Option<Stmt> {
        let expr = self.parse_expression(&Precedence::Lowest);
        expr.map(Stmt::Expr)
    }

    fn parse_block_statement(&mut self) -> Stmt {
        self.next_token();
        let mut stmts = Vec::new();
        while self.cur_token.clone().unwrap() != Token::RBrace {
            let stmt = self.parse_statement();
            if let Some(stmt) = stmt {
                stmts.push(stmt);
            }
            self.next_token();
        }
        Stmt::Block(stmts)
    }

    fn parse_expression(&mut self, precedence: &Precedence) -> Option<Expr> {
        let mut left_expr = match self.cur_token.clone() {
            Some(Token::Int(_)) => self.parse_integer_literal(),
            Some(Token::True | Token::False) => self.parse_boolean_literal(),
            Some(Token::Ident(_)) => self.parse_identifier(),
            Some(Token::Bang | Token::Minus) => self.parse_prefix_expression(),
            Some(Token::LParen) => self.parse_grouped_expression(),
            Some(Token::If) => self.parse_if_expression(),
            Some(Token::Function) => self.parse_function_literal(),
            _ => None,
        };

        while self.peek_token.clone().unwrap() != Token::SemiColon
            && *precedence < self.peek_precedence()
        {
            self.next_token();
            match self.cur_token.clone() {
                Some(
                    Token::Plus
                    | Token::Minus
                    | Token::Slash
                    | Token::Asterisk
                    | Token::LessThan
                    | Token::GreaterThan
                    | Token::Eq
                    | Token::NotEq,
                ) => {
                    left_expr = self.parse_infix_expression(left_expr.unwrap());
                }
                Some(Token::LParen) => {
                    left_expr = self.parse_call_expression(left_expr.unwrap());
                }
                _ => break,
            }
        }

        left_expr
    }

    fn parse_prefix_expression(&mut self) -> Option<Expr> {
        match self.cur_token.clone() {
            Some(Token::Bang | Token::Minus) => {
                let op = self.cur_token.clone().unwrap().to_string();
                self.next_token();
                let expr = self.parse_expression(&Precedence::Prefix).unwrap();
                Some(Expr::Prefix(op, Box::new(expr)))
            }
            _ => None,
        }
    }

    fn parse_infix_expression(&mut self, left: Expr) -> Option<Expr> {
        match self.cur_token.clone() {
            Some(
                Token::Plus
                | Token::Minus
                | Token::Slash
                | Token::Asterisk
                | Token::LessThan
                | Token::GreaterThan
                | Token::Eq
                | Token::NotEq,
            ) => {
                let op = self.cur_token.clone().unwrap().to_string();
                let precedence = self.cur_precedence();
                self.next_token();
                let right = self.parse_expression(&precedence).unwrap();
                Some(Expr::Infix(Box::new(left), op, Box::new(right)))
            }
            _ => None,
        }
    }

    fn parse_grouped_expression(&mut self) -> Option<Expr> {
        self.next_token();
        let expr = self.parse_expression(&Precedence::Lowest);
        if self.peek_token.clone().unwrap() == Token::RParen {
            self.next_token();
            return expr;
        }
        None
    }

    fn parse_if_expression(&mut self) -> Option<Expr> {
        if let Some(Token::LParen) = self.peek_token.clone() {
            self.next_token();
            self.next_token();
            let cond = self.parse_expression(&Precedence::Lowest).unwrap();
            if let Some(Token::RParen) = self.peek_token.clone() {
                self.next_token();
                if let Some(Token::LBrace) = self.peek_token.clone() {
                    self.next_token();
                    let consequence = self.parse_block_statement();
                    let alternative = if let Some(Token::Else) = self.peek_token.clone() {
                        self.next_token();
                        if let Some(Token::LBrace) = self.peek_token.clone() {
                            self.next_token();
                            Some(self.parse_block_statement())
                        } else {
                            self.errors.push("expected { after else".to_string());
                            return None;
                        }
                    } else {
                        None
                    };
                    return Some(Expr::If(
                        Box::new(cond),
                        Box::new(consequence),
                        alternative.map(Box::new),
                    ));
                }
                self.errors
                    .push("expected { after if condition".to_string());
                return None;
            }
            self.errors
                .push("expected ) after if condition".to_string());
            return None;
        }
        self.errors.push("expected ( after if".to_string());
        None
    }

    fn parse_call_expression(&mut self, function: Expr) -> Option<Expr> {
        if let Some(Token::LParen) = self.cur_token.clone() {
            let args = self.parse_call_arguments();
            Some(Expr::Call(Box::new(function), args))
        } else {
            None
        }
    }

    fn parse_call_arguments(&mut self) -> Vec<Expr> {
        let mut args = Vec::new();
        if self.peek_token.clone().unwrap() == Token::RParen {
            self.next_token();
            return args;
        }
        self.next_token();
        args.push(self.parse_expression(&Precedence::Lowest).unwrap());
        while self.peek_token.clone().unwrap() == Token::Comma {
            self.next_token();
            self.next_token();
            args.push(self.parse_expression(&Precedence::Lowest).unwrap());
        }
        if self.peek_token.clone().unwrap() != Token::RParen {
            self.errors
                .push("expected , or ) after call arguments".to_string());
        }
        self.next_token();
        args
    }

    fn parse_identifier(&mut self) -> Option<Expr> {
        match self.cur_token.clone() {
            Some(Token::Ident(ident)) => Some(Expr::Ident(Ident(ident))),
            _ => None,
        }
    }

    fn parse_integer_literal(&mut self) -> Option<Expr> {
        match self.cur_token.clone() {
            Some(Token::Int(int)) => Some(Expr::Literal(Literal::Int(int))),
            _ => None,
        }
    }

    fn parse_boolean_literal(&mut self) -> Option<Expr> {
        match self.cur_token.clone() {
            Some(Token::True) => Some(Expr::Literal(Literal::Bool(true))),
            Some(Token::False) => Some(Expr::Literal(Literal::Bool(false))),
            _ => None,
        }
    }

    fn parse_function_literal(&mut self) -> Option<Expr> {
        if let Some(Token::LParen) = self.peek_token.clone() {
            self.next_token();
            let params = self.parse_function_parameters();
            self.next_token();
            if let Some(Token::LBrace) = self.peek_token.clone() {
                self.next_token();
                let body = self.parse_block_statement();
                return Some(Expr::Function(params, Box::new(body)));
            }
            self.errors
                .push("expected { after function parameters".to_string());
            return None;
        }
        self.errors.push("expected ( after function".to_string());
        None
    }

    fn parse_function_parameters(&mut self) -> Vec<Ident> {
        let mut params = Vec::new();
        if self.peek_token.clone().unwrap() == Token::RParen {
            self.next_token();
            return params;
        }
        self.next_token();
        if let Some(Token::Ident(ident)) = self.cur_token.clone() {
            params.push(Ident(ident));
        }
        while self.peek_token.clone().unwrap() == Token::Comma {
            self.next_token();
            self.next_token();
            if let Some(Token::Ident(ident)) = self.cur_token.clone() {
                params.push(Ident(ident));
            }
        }
        if self.peek_token.clone().unwrap() != Token::RParen {
            self.errors
                .push("expected , or ) after function parameters".to_string());
        }
        params
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{Expr, Literal, Stmt},
        lexer::Lexer,
    };

    use super::Parser;

    fn check_parser_errors(parser: &Parser) {
        let errors = parser.errors.clone();
        if errors.is_empty() {
            return;
        }
        println!("parser has {} errors", errors.len());
        for error in errors {
            println!("parser error: {error}");
        }
        panic!("parser error");
    }

    #[test]
    fn test_let_statement() {
        let input = "
        let x = 5;
        let y = 10;
        let foobar = 838383;
        ";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);
        assert_eq!(program.len(), 3);

        let tests = ["x", "y", "foobar"];
        let literals = [5, 10, 838_383];
        for (i, test) in tests.iter().enumerate() {
            let stmt = &program[i];
            match stmt {
                Stmt::Let(ident, expr) => {
                    assert_eq!(ident.0, *test);
                    match expr {
                        Expr::Literal(literal) => match literal {
                            Literal::Int(int) => {
                                assert_eq!(*int, literals[i]);
                            }
                            Literal::Bool(_) => panic!("expected integer literal"),
                        },
                        _ => panic!("expected literal expression"),
                    }
                }
                _ => panic!("expected let statement"),
            }
        }
    }

    #[test]
    fn test_return_statement() {
        let input = "
        return 5;
        return 10;
        return 993322;
        ";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);
        assert_eq!(program.len(), 3);

        let tests = [5, 10, 993_322];
        for (i, test) in tests.iter().enumerate() {
            let stmt = &program[i];
            match stmt {
                Stmt::Return(expr) => match expr {
                    Expr::Literal(literal) => match literal {
                        Literal::Int(int) => {
                            assert_eq!(*int, *test);
                        }
                        Literal::Bool(_) => panic!("expected integer literal"),
                    },
                    _ => panic!("expected literal expression"),
                },
                _ => panic!("expected return statement"),
            }
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);
        assert_eq!(program.len(), 1);

        let stmt = &program[0];
        match stmt {
            Stmt::Expr(expr) => match expr {
                Expr::Ident(ident) => {
                    assert_eq!(ident.0, "foobar");
                }
                _ => panic!("expected identifier expression"),
            },
            _ => panic!("expected expression statement"),
        }
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);
        assert_eq!(program.len(), 1);

        let stmt = &program[0];
        match stmt {
            Stmt::Expr(expr) => match expr {
                Expr::Literal(literal) => match literal {
                    Literal::Int(int) => {
                        assert_eq!(*int, 5);
                    }
                    Literal::Bool(_) => panic!("expected integer literal"),
                },
                _ => panic!("expected literal expression"),
            },
            _ => panic!("expected expression statement"),
        }
    }

    #[test]
    fn test_boolean_literal_expression() {
        let input = "true; false;";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);
        assert_eq!(program.len(), 2);

        let tests = [true, false];
        for (i, test) in tests.iter().enumerate() {
            let stmt = &program[i];
            match stmt {
                Stmt::Expr(expr) => match expr {
                    Expr::Literal(literal) => match literal {
                        Literal::Bool(bool) => {
                            assert_eq!(*bool, *test);
                        }
                        Literal::Int(_) => panic!("expected boolean literal"),
                    },
                    _ => panic!("expected literal expression"),
                },
                _ => panic!("expected expression statement"),
            }
        }
    }

    #[test]
    fn test_prefix_expression() {
        let input = "!5; -15;";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);
        assert_eq!(program.len(), 2);

        let tests = [5, 15];
        let operators = ["!", "-"];
        for (i, test) in tests.iter().enumerate() {
            let stmt = &program[i];
            match stmt {
                Stmt::Expr(expr) => match expr {
                    Expr::Prefix(op, expr) => {
                        assert_eq!(*op, operators[i]);
                        match expr.as_ref() {
                            Expr::Literal(literal) => match literal {
                                Literal::Int(int) => {
                                    assert_eq!(*int, *test);
                                }
                                Literal::Bool(_) => panic!("expected integer literal"),
                            },
                            _ => panic!("expected literal expression"),
                        }
                    }
                    _ => panic!("expected prefix expression"),
                },
                _ => panic!("expected expression statement"),
            }
        }
    }

    #[test]
    fn test_infix_expression() {
        let input = "5 + 5; 5 - 5; 5 * 5; 5 / 5; 5 > 5; 5 < 5; 5 == 5; 5 != 5;";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);
        assert_eq!(program.len(), 8);

        let tests = [5, 5, 5, 5, 5, 5, 5, 5];
        let operators = ["+", "-", "*", "/", ">", "<", "==", "!="];
        for (i, test) in tests.iter().enumerate() {
            let stmt = &program[i];
            match stmt {
                Stmt::Expr(expr) => match expr {
                    Expr::Infix(left, op, right) => {
                        assert_eq!(*op, operators[i]);
                        match left.as_ref() {
                            Expr::Literal(literal) => match literal {
                                Literal::Int(int) => {
                                    assert_eq!(*int, *test);
                                }
                                Literal::Bool(_) => panic!("expected integer literal"),
                            },
                            _ => panic!("expected literal expression"),
                        }
                        match right.as_ref() {
                            Expr::Literal(literal) => match literal {
                                Literal::Int(int) => {
                                    assert_eq!(*int, *test);
                                }
                                Literal::Bool(_) => panic!("expected integer literal"),
                            },
                            _ => panic!("expected literal expression"),
                        }
                    }
                    _ => panic!("expected infix expression"),
                },
                _ => panic!("expected expression statement"),
            }
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let tests = [
            "-a * b;",
            "!-a;",
            "a + b + c;",
            "a + b - c;",
            "a * b * c;",
            "a * b / c;",
            "a + b / c;",
            "a + b * c + d / e - f;",
            "5 > 4 == 3 < 4;",
            "5 < 4 != 3 > 4;",
            "3 + 4 * 5 == 3 * 1 + 4 * 5;",
            "3 + 4 * 5 == 3 * 1 + 4 * 5;",
            "1 + (2 + 3) + 4;",
            "(5 + 5) * 2;",
            "2 / (5 + 5);",
            "-(5 + 5);",
            "!(true == true);",
        ];
        let expected = [
            "((-a) * b)",
            "(!(-a))",
            "((a + b) + c)",
            "((a + b) - c)",
            "((a * b) * c)",
            "((a * b) / c)",
            "(a + (b / c))",
            "(((a + (b * c)) + (d / e)) - f)",
            "((5 > 4) == (3 < 4))",
            "((5 < 4) != (3 > 4))",
            "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            "((1 + (2 + 3)) + 4)",
            "((5 + 5) * 2)",
            "(2 / (5 + 5))",
            "(-(5 + 5))",
            "(!(true == true))",
        ];
        for (i, test) in tests.iter().enumerate() {
            let lexer = Lexer::new((*test).to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(&parser);
            assert_eq!(program.len(), 1);
            let stmt = &program[0];
            match stmt {
                Stmt::Expr(expr) => {
                    assert_eq!(format!("{expr}"), expected[i]);
                }
                _ => panic!("expected expression statement"),
            }
        }
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x };";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);
        assert_eq!(program.len(), 1);
        let stmt = &program[0];
        match stmt {
            Stmt::Expr(expr) => match expr {
                Expr::If(cond, consequence, alternative) => {
                    assert_eq!(format!("{cond}"), "(x < y)");
                    assert_eq!(format!("{consequence}"), "{\n  x;\n}");
                    assert_eq!(*alternative, None);
                }
                _ => panic!("expected if expression"),
            },
            _ => panic!("expected expression statement"),
        }
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y };";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);
        assert_eq!(program.len(), 1);
        let stmt = &program[0];
        match stmt {
            Stmt::Expr(expr) => match expr {
                Expr::If(cond, consequence, alternative) => {
                    assert_eq!(format!("{cond}"), "(x < y)");
                    assert_eq!(format!("{consequence}"), "{\n  x;\n}");
                    match alternative {
                        Some(alt) => {
                            assert_eq!(format!("{alt}"), "{\n  y;\n}");
                        }
                        None => panic!("expected alternative"),
                    }
                }
                _ => panic!("expected if expression"),
            },
            _ => panic!("expected expression statement"),
        }
    }

    #[test]
    fn test_function_expression() {
        let input = "fn(x, y) { x + y; };";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);
        assert_eq!(program.len(), 1);
        let stmt = &program[0];
        match stmt {
            Stmt::Expr(expr) => match expr {
                Expr::Function(params, body) => {
                    assert_eq!(params.len(), 2);
                    assert_eq!(params[0].0, "x");
                    assert_eq!(params[1].0, "y");
                    assert_eq!(format!("{body}"), "{\n  (x + y);\n}");
                }
                _ => panic!("expected function expression"),
            },
            _ => panic!("expected expression statement"),
        }
    }

    #[test]
    fn test_call_expression() {
        let input = "add(1, 2 * 3, 4 + 5);";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);
        assert_eq!(program.len(), 1);
        let stmt = &program[0];
        match stmt {
            Stmt::Expr(expr) => match expr {
                Expr::Call(func, args) => {
                    assert_eq!(format!("{func}"), "add");
                    assert_eq!(args.len(), 3);
                    assert_eq!(format!("{}", args[0]), "1");
                    assert_eq!(format!("{}", args[1]), "(2 * 3)");
                    assert_eq!(format!("{}", args[2]), "(4 + 5)");
                }
                _ => panic!("expected call expression"),
            },
            _ => panic!("expected expression statement"),
        }
    }
}
