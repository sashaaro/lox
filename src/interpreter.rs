use std::collections::HashMap;
use crate::ast::{Expr, Literal, Stmt};
use crate::token::TokenType;
use std::io::Write;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
}

pub struct Interpreter {
    env: Environment,
}
impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            env: Environment::default(),
        }
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
            Expr::Variable(token) => {
                let name = &token.lexeme;
                self.env
                    .get(name)
                    .ok_or_else(|| format!("Undefined variable '{}'", name))
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

    pub fn interpret_statements<W: Write>(
        &mut self,
        statements: &[Stmt],
        output: &mut W,
    ) -> Result<(), String> {
        for stmt in statements {
            self.execute(stmt, output)?;
        }
        Ok(())
    }

    fn execute<W: Write>(&mut self, stmt: &Stmt, output: &mut W) -> Result<(), String> {
        match stmt {
            Stmt::Print(expr) => {
                let value = self.evaluate(expr)?;
                writeln!(output, "{}", self.stringify(&value)).map_err(|e| e.to_string())?;
                Ok(())
            }
            Stmt::Expression(expr) => {
                self.evaluate(expr)?;
                Ok(())
            }
            Stmt::Var { name, initializer } => {
                let value = if let Some(expr) = initializer {
                    self.evaluate(expr)?
                } else {
                    Value::Nil
                };
                self.env.define(name.lexeme.clone(), value);
                Ok(())
            }
        }
    }

    fn stringify(&self, val: &Value) -> String {
        match val {
            Value::Nil => "nil".into(),
            Value::Boolean(b) => b.to_string(),
            Value::Number(n) => {
                if n.fract() == 0.0 {
                    format!("{}", *n as i64)
                } else {
                    format!("{}", n)
                }
            }
            Value::String(s) => s.clone(),
        }
    }
}

#[derive(Default)]
pub struct Environment {
    values: HashMap<String, Value>,
}

impl Environment {
    pub fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        self.values.get(name).cloned()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;
    use crate::scanner::Scanner;
    use crate::ast::{Stmt, Expr, Literal};

    fn run_and_capture(source: &str) -> Vec<String> {
        let tokens = Scanner::new(source).scan_tokens();
        let mut parser = Parser::new(tokens);
        let statements = parser.parse();

        let mut output = Vec::new();
        let mut interpreter = Interpreter::new();
        interpreter
            .interpret_statements(&statements, &mut output)
            .unwrap();

        let result = String::from_utf8_lossy(&output).to_string();
        result
            .lines()
            .map(str::to_string)
            .filter(|s| !s.trim().is_empty())
            .collect()
    }

    #[test]
    fn test_arithmetic() {
        let result = run_and_capture("print 1 + 2 * 3;");
        assert_eq!(result, vec!["7"]);

        let result = run_and_capture("print (1 + 2) * 3;");
        assert_eq!(result, vec!["9"]);
    }

    #[test]
    fn test_unary() {
        assert_eq!(run_and_capture("print -5;"), vec!["-5"]);
        assert_eq!(run_and_capture("print !false;"), vec!["true"]);
    }

    #[test]
    fn test_variable_definition_and_use() {
        assert_eq!(run_and_capture("var x = 42; print x;"), vec!["42"]);
    }

    #[test]
    fn test_string_concat() {
        assert_eq!(
            run_and_capture("print \"hello \" + \"world\";"),
            vec!["hello world"]
        );
    }
}