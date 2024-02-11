use crate::parser::ast::{Expr, Infix, Literal, Prefix, Program, Stmt};

use self::object::Object;

mod object;

pub struct Evaluator {}

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
        Evaluator {}
    }

    pub fn eval_program(&mut self, program: Program) -> Object {
        let result = self.eval_block_statement(program);
        return_value(result)
    }

    pub fn eval_block_statement(&mut self, program: Program) -> Object {
        let mut result = Object::Null;
        for statement in program {
            result = self.eval_statement(statement);
            if let Object::ReturnValue(_) = result {
                return result;
            }
        }
        result
    }

    pub fn eval_statement(&mut self, stmt: Stmt) -> Object {
        match stmt {
            Stmt::Expr(expr) => self.eval_expression(expr),
            Stmt::Block(stmts) => self.eval_block_statement(stmts),
            _ => Object::Null,
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
            _ => Object::Null,
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
        match operator {
            Prefix::Not => match right {
                Object::Boolean(value) => Object::Boolean(!value),
                Object::Integer(int) => Object::Boolean(int == 0),
                _ => Object::Null,
            },
            Prefix::Minus => match right {
                Object::Integer(value) => Object::Integer(-value),
                _ => Object::Null,
            },
        }
    }

    pub fn eval_infix_expression(&mut self, left: Expr, operator: &Infix, right: Expr) -> Object {
        let left = self.eval_expression(left);
        let right = self.eval_expression(right);
        match (left, right) {
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
                _ => Object::Null,
            },
            _ => Object::Null,
        }
    }

    pub fn eval_if_else_expression(
        &mut self,
        cond: &Expr,
        consequence: Stmt,
        alternative: Option<Box<Stmt>>,
    ) -> Object {
        let cond = self.eval_expression(cond.clone());
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
}
