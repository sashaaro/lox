use crate::ast::{Expr, Literal};
use crate::token::TokenType;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
}

pub struct Interpreter;

impl Interpreter {
    pub fn new() -> Self {
        Interpreter
    }

    pub fn interpret(&self, expr: &Expr) -> Result<Value, String> {
        self.evaluate(expr)
    }

    fn evaluate(&self, expr: &Expr) -> Result<Value, String> {
        match expr {
            Expr::Literal(lit) => Ok(self.literal_to_value(lit)),
            Expr::Grouping(inner) => self.evaluate(inner),
            Expr::Unary { operator, right } => {
                let right = self.evaluate(right)?;
                match operator.kind {
                    TokenType::Minus => {
                        if let Value::Number(n) = right {
                            Ok(Value::Number(-n))
                        } else {
                            Err("Unary '-' applied to non-number.".into())
                        }
                    }
                    TokenType::Bang => Ok(Value::Boolean(!self.is_truthy(&right))),
                    _ => Err(format!("Unknown unary operator {:?}", operator.kind)),
                }
            }
            Expr::Binary { left, operator, right } => {
                let left = self.evaluate(left)?;
                let right = self.evaluate(right)?;
                match operator.kind {
                    TokenType::Plus => match (left, right) {
                        (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a + b)),
                        (Value::String(a), Value::String(b)) => Ok(Value::String(a + &b)),
                        _ => Err("Operands of '+' must be two numbers or two strings.".into()),
                    },
                    TokenType::Minus => self.binary_number_op(left, right, |a, b| a - b),
                    TokenType::Star => self.binary_number_op(left, right, |a, b| a * b),
                    TokenType::Slash => self.binary_number_op(left, right, |a, b| a / b),

                    TokenType::Greater => self.binary_cmp_op(left, right, |a, b| a > b),
                    TokenType::GreaterEqual => self.binary_cmp_op(left, right, |a, b| a >= b),
                    TokenType::Less => self.binary_cmp_op(left, right, |a, b| a < b),
                    TokenType::LessEqual => self.binary_cmp_op(left, right, |a, b| a <= b),

                    TokenType::EqualEqual => Ok(Value::Boolean(left == right)),
                    TokenType::BangEqual => Ok(Value::Boolean(left != right)),

                    _ => Err(format!("Unknown binary operator {:?}", operator.kind)),
                }
            }
        }
    }

    fn binary_number_op<F>(&self, left: Value, right: Value, op: F) -> Result<Value, String>
    where
        F: Fn(f64, f64) -> f64,
    {
        match (left, right) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(op(a, b))),
            _ => Err("Binary operation requires two numbers.".into()),
        }
    }

    fn binary_cmp_op<F>(&self, left: Value, right: Value, op: F) -> Result<Value, String>
    where
        F: Fn(f64, f64) -> bool,
    {
        match (left, right) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(op(a, b))),
            _ => Err("Comparison requires two numbers.".into()),
        }
    }

    fn is_truthy(&self, val: &Value) -> bool {
        match val {
            Value::Nil => false,
            Value::Boolean(b) => *b,
            _ => true,
        }
    }

    fn literal_to_value(&self, lit: &Literal) -> Value {
        match lit {
            Literal::Number(n) => Value::Number(*n),
            Literal::String(s) => Value::String(s.clone()),
            Literal::Boolean(b) => Value::Boolean(*b),
            Literal::Nil => Value::Nil,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;
    use crate::scanner::Scanner;

    fn run(source: &str) -> Value {
        let tokens = Scanner::new(source).scan_tokens();
        let mut parser = Parser::new(tokens);
        let expr = parser.parse().expect("Parse failed");
        Interpreter::new().interpret(&expr).expect("Interpret failed")
    }

    #[test]
    fn test_arithmetic() {
        assert_eq!(run("1 + 2 * 3"), Value::Number(7.0));
        assert_eq!(run("(1 + 2) * 3"), Value::Number(9.0));
    }

    #[test]
    fn test_unary() {
        assert_eq!(run("-5"), Value::Number(-5.0));
        assert_eq!(run("!false"), Value::Boolean(true));
    }

    #[test]
    fn test_equality() {
        assert_eq!(run("1 == 1"), Value::Boolean(true));
        assert_eq!(run("2 != 3"), Value::Boolean(true));
        assert_eq!(run("1 < 2"), Value::Boolean(true));
    }
}