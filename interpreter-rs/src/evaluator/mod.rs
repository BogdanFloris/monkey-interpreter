use std::{cell::RefCell, rc::Rc};

use crate::parser::ast::{Expr, Ident, Infix, Literal, Prefix, Program, Stmt};

use self::{enviroment::Environment, object::Object};

mod enviroment;
mod object;

pub struct Evaluator {
    env: Rc<RefCell<Environment>>,
}

impl Default for Evaluator {
    fn default() -> Self {
        Self::new()
    }
}

fn return_value(obj: Object) -> Object {
    match obj {
        Object::ReturnValue(val) => *val,
        _ => obj,
    }
}

impl Evaluator {
    #[must_use]
    pub fn new() -> Self {
        Evaluator {
            env: Rc::new(RefCell::new(Environment::new())),
        }
    }

    pub fn eval_program(&mut self, program: Program) -> Object {
        let result = self.eval_block_statement(program);
        return_value(result)
    }

    pub fn eval_block_statement(&mut self, program: Program) -> Object {
        let mut result = Object::Null;
        for statement in program {
            result = self.eval_statement(statement);
            match result {
                Object::ReturnValue(_) | Object::Error(_) => return result,
                _ => {}
            }
        }
        result
    }

    pub fn eval_statement(&mut self, stmt: Stmt) -> Object {
        match stmt {
            Stmt::Expr(expr) => self.eval_expression(expr),
            Stmt::Block(stmts) => self.eval_block_statement(stmts),
            Stmt::Return(expr) => {
                let value = self.eval_expression(expr);
                if let Object::Error(_) = value {
                    return value;
                }
                Object::ReturnValue(Box::new(value))
            }
            Stmt::Let(ident, expr) => {
                let value = self.eval_expression(expr);
                if let Object::Error(_) = value {
                    return value;
                }
                self.env.borrow_mut().set(&ident.0, value.clone());
                value
            }
        }
    }

    pub fn eval_expression(&mut self, expr: Expr) -> Object {
        match expr {
            Expr::Literal(literal) => self.eval_literal(&literal),
            Expr::Prefix(prefix, right) => self.eval_prefix_expression(&prefix, &right),
            Expr::Infix(left, infix, right) => self.eval_infix_expression(*left, &infix, *right),
            Expr::If(cond, consequence, alternative) => {
                self.eval_if_else_expression(&cond, *consequence, alternative)
            }
            Expr::Ident(ident) => self.eval_identifier(&ident.0),
            Expr::Function(params, body) => self.eval_function(params, *body),
            Expr::Call(func, args) => self.eval_call_expression(*func, args),
        }
    }

    fn eval_call_expression(&mut self, func: Expr, args: Vec<Expr>) -> Object {
        let func = self.eval_expression(func);
        if let Object::Error(_) = func {
            return func;
        }
        let args = args
            .into_iter()
            .map(|arg| self.eval_expression(arg))
            .collect::<Vec<Object>>();
        if args.iter().any(|arg| matches!(arg, Object::Error(_))) {
            return Object::Error("error evaluating arguments".to_string());
        }
        match func {
            Object::Function(params, body, env) => {
                if params.len() != args.len() {
                    return Object::Error(format!(
                        "wrong number of arguments: want={}, got={}",
                        params.len(),
                        args.len()
                    ));
                }
                let mut extended_env = Environment::new_with_parent(Rc::clone(&env));
                for (param, arg) in params.iter().zip(args) {
                    extended_env.set(&param.0, arg);
                }
                let mut evaluator = Evaluator {
                    env: Rc::new(RefCell::new(extended_env)),
                };
                let result = evaluator.eval_block_statement(body);
                return_value(result)
            }
            _ => Object::Error(format!("{func} is not a function")),
        }
    }

    fn eval_function(&mut self, params: Vec<Ident>, body: Stmt) -> Object {
        match body {
            Stmt::Block(program) => Object::Function(params, program, Rc::clone(&self.env)),
            _ => Object::Error("function body must be a block statement".to_string()),
        }
    }

    fn eval_identifier(&self, ident: &str) -> Object {
        match self.env.borrow().get(ident) {
            Some(val) => val,
            None => Object::Error(format!("identifier not found: {ident}").to_string()),
        }
    }

    pub fn eval_literal(&mut self, literal: &Literal) -> Object {
        match literal {
            Literal::Int(value) => Object::Integer(*value),
            Literal::Bool(value) => Object::Boolean(*value),
        }
    }

    pub fn eval_prefix_expression(&mut self, operator: &Prefix, right: &Expr) -> Object {
        let right = self.eval_expression(right.clone());
        if let Object::Error(_) = right {
            return right;
        }
        match operator {
            Prefix::Not => match right {
                Object::Boolean(value) => Object::Boolean(!value),
                Object::Integer(int) => Object::Boolean(int == 0),
                _ => Object::Error(format!("unknown operator: !{right:?}").to_string()),
            },
            Prefix::Minus => match right {
                Object::Integer(value) => Object::Integer(-value),
                _ => Object::Error(format!("unknown operator: -{right:?}").to_string()),
            },
        }
    }

    pub fn eval_infix_expression(&mut self, left: Expr, operator: &Infix, right: Expr) -> Object {
        let left = self.eval_expression(left);
        if let Object::Error(_) = left {
            return left;
        }
        let right = self.eval_expression(right);
        if let Object::Error(_) = right {
            return right;
        }
        match (left.clone(), right.clone()) {
            (Object::Integer(left), Object::Integer(right)) => match operator {
                Infix::Plus => Object::Integer(left + right),
                Infix::Minus => Object::Integer(left - right),
                Infix::Multiply => Object::Integer(left * right),
                Infix::Divide => Object::Integer(left / right),
                Infix::GreaterThan => Object::Boolean(left > right),
                Infix::LessThan => Object::Boolean(left < right),
                Infix::Equal => Object::Boolean(left == right),
                Infix::NotEqual => Object::Boolean(left != right),
            },
            (Object::Boolean(left), Object::Boolean(right)) => match operator {
                Infix::Equal => Object::Boolean(left == right),
                Infix::NotEqual => Object::Boolean(left != right),
                _ => Object::Error(
                    format!("unknown operator: Boolean({left:?}) {operator} Boolean({right:?})")
                        .to_string(),
                ),
            },
            _ => Object::Error(format!("type mismatch: {left:?} {operator} {right:?}").to_string()),
        }
    }

    pub fn eval_if_else_expression(
        &mut self,
        cond: &Expr,
        consequence: Stmt,
        alternative: Option<Box<Stmt>>,
    ) -> Object {
        let cond = self.eval_expression(cond.clone());
        if let Object::Error(_) = cond {
            return cond;
        }
        if cond.is_truthy() {
            self.eval_statement(consequence)
        } else {
            match alternative {
                Some(alternative) => self.eval_statement(*alternative),
                None => Object::Null,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{lexer::Lexer, parser::Parser};

    use super::*;

    #[test]
    fn test_eval_literal_expression() {
        let tests = vec![("5", 5), ("10", 10)];

        for (input, expected) in tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            let mut evaluator = Evaluator::new();
            let result = evaluator.eval_program(program);
            assert_eq!(result, Object::Integer(expected));
        }

        let tests = vec![("true", true), ("false", false)];

        for (input, expected) in tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            let mut evaluator = Evaluator::new();
            let result = evaluator.eval_program(program);
            assert_eq!(result, Object::Boolean(expected));
        }
    }

    #[test]
    fn test_boolean_prefix_expression() {
        let tests = vec![
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!5", true),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            let mut evaluator = Evaluator::new();
            let result = evaluator.eval_program(program);
            assert_eq!(result, Object::Boolean(expected));
        }
    }

    #[test]
    fn test_integer_prefix_expression() {
        let tests = vec![("-5", -5), ("-10", -10), ("5", 5), ("10", 10)];

        for (input, expected) in tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            let mut evaluator = Evaluator::new();
            let result = evaluator.eval_program(program);
            assert_eq!(result, Object::Integer(expected));
        }
    }

    #[test]
    fn test_integer_infix_expression() {
        let tests = vec![("5 + 5", 10), ("5 - 5", 0), ("5 * 5", 25), ("5 / 5", 1)];

        for (input, expected) in tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            let mut evaluator = Evaluator::new();
            let result = evaluator.eval_program(program);
            assert_eq!(result, Object::Integer(expected));
        }
    }

    #[test]
    fn test_boolean_infix_expression() {
        let tests = vec![
            ("true == true", true),
            ("true != true", false),
            ("true == false", false),
            ("true != false", true),
            ("false == false", true),
            ("false != false", false),
            ("5 < 10", true),
            ("5 > 10", false),
            ("5 < 5", false),
            ("5 > 5", false),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            let mut evaluator = Evaluator::new();
            let result = evaluator.eval_program(program);
            assert_eq!(result, Object::Boolean(expected));
        }
    }

    #[test]
    fn test_if_else_expression() {
        let tests = vec![
            ("if (true) { 10 }", Some(10)),
            ("if (false) { 10 }", None),
            ("if (1) { 10 }", Some(10)),
            ("if (1 < 2) { 10 }", Some(10)),
            ("if (1 > 2) { 10 }", None),
            ("if (1 > 2) { 10 } else { 20 }", Some(20)),
            ("if (1 < 2) { 10 } else { 20 }", Some(10)),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            let mut evaluator = Evaluator::new();
            let result = evaluator.eval_program(program);
            if let Some(expected) = expected {
                assert_eq!(result, Object::Integer(expected));
            } else {
                assert_eq!(result, Object::Null);
            }
        }
    }

    #[test]
    fn test_return_statements() {
        let tests = vec![
            ("return 10;", 10),
            ("return 10; 9;", 10),
            ("return 2 * 5; 9;", 10),
            ("9; return 2 * 5; 9;", 10),
            ("if (10 > 1) { if (10 > 1) { return 10; } } return 1;", 10),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            let mut evaluator = Evaluator::new();
            let result = evaluator.eval_program(program);
            assert_eq!(result, Object::Integer(expected));
        }
    }

    #[test]
    fn test_error_handling() {
        let tests = vec![
            ("5 + true;", "type mismatch: Integer(5) + Boolean(true)"),
            ("-true", "unknown operator: -Boolean(true)"),
            (
                "true + false;",
                "unknown operator: Boolean(true) + Boolean(false)",
            ),
            (
                "5; true + false; 5",
                "unknown operator: Boolean(true) + Boolean(false)",
            ),
            (
                "if (10 > 1) { if (10 > 1) { return true + false; } } return 1;",
                "unknown operator: Boolean(true) + Boolean(false)",
            ),
            ("foobar", "identifier not found: foobar"),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            let mut evaluator = Evaluator::new();
            let result = evaluator.eval_program(program);
            assert_eq!(result, Object::Error(expected.to_string()));
        }
    }

    #[test]
    fn test_let_statements() {
        let tests = vec![
            ("let a = 5; a;", 5),
            ("let a = 5 * 5; a;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];
        for (input, expected) in tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            let mut evaluator = Evaluator::new();
            let result = evaluator.eval_program(program);
            assert_eq!(result, Object::Integer(expected));
        }
    }

    #[test]
    fn test_function_object() {
        let input = "fn(x) { x + 2; }";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let mut evaluator = Evaluator::new();
        let result = evaluator.eval_program(program);
        assert_eq!(result.to_string(), "fn(x) {\n  (x + 2);\n}");
    }

    #[test]
    fn test_function_application() {
        let tests = vec![
            ("let identity = fn(x) { x; }; identity(5);", 5),
            ("let identity = fn(x) { return x; }; identity(5);", 5),
            ("let double = fn(x) { x * 2; }; double(5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
            ("fn(x) { x; }(5)", 5),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            let mut evaluator = Evaluator::new();
            let result = evaluator.eval_program(program);
            assert_eq!(result, Object::Integer(expected));
        }
    }
}
