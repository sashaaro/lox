use crate::ast::{Expr, Literal, Stmt};
use crate::token::TokenType;
use std::cell::RefCell;
use std::collections::HashMap;
use std::io::Write;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
    Function(Rc<LoxFunction>),
}

#[derive(Debug)]
pub enum ReturnValue {
    Return(Value),
    Runtime(String),
}


#[derive(Debug)]
pub struct LoxFunction {
    pub name: String,
    pub params: Vec<String>,
    pub body: Vec<Stmt>,
    pub closure: Rc<RefCell<Environment>>,
}

impl PartialEq for LoxFunction {
    fn eq(&self, other: &Self) -> bool {
        todo!()
    }

    fn ne(&self, other: &Self) -> bool {
        todo!()
    }
}

impl LoxFunction {
    pub fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>) -> Result<Value, String> {
        let mut new_env = Environment::with_enclosing(self.closure.clone());

        for (param, arg) in self.params.iter().zip(args.into_iter()) {
            new_env.borrow_mut().define(param.clone(), arg);
        }

        let result = interpreter.execute_block_with_return(&self.body, new_env);
        match result {
            Ok(()) => {
                Ok(Value::Nil)
            }
            Err(ReturnValue::Return(val)) => {
                Ok(val)
            }
            Err(ReturnValue::Runtime(e)) => Err(e),
        }
    }
}

pub struct Interpreter {
    env: Rc<RefCell<Environment>>,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            env: Rc::new(RefCell::new(Environment::default())),
        }
    }

    pub fn interpret(&mut self, expr: &Expr) -> Result<Value, String> {
        self.evaluate(expr)
    }

    fn evaluate(&mut self, expr: &Expr) -> Result<Value, String> {
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
                    .borrow_mut()
                    .get(name)
                    .ok_or_else(|| format!("Undefined variable '{}'", name))
            }
            Expr::Binary {
                left,
                operator,
                right,
            } => {
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
            Expr::Assign { name, value } => {
                let val = self.evaluate(value)?;

                self.env
                    .borrow_mut()
                    .assign(&name.lexeme, val.clone())
                    .ok_or_else(|| format!("Undefined variable '{}'", name.lexeme))?;
                Ok(val)
            },
            Expr::Call { callee, arguments, .. } => {
                let callee_val = self.evaluate(callee)?;
                let mut args = Vec::new();
                for arg in arguments {
                    args.push(self.evaluate(arg)?);
                }

                match callee_val {
                    Value::Function(f) => f.call(self, args),
                    _ => Err("Can only call functions.".into()),
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
            Value::Number(0.0) => false,
            Value::String(s) => !s.is_empty(),
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
                self.env.borrow_mut().define(name.lexeme.clone(), value);
                Ok(())
            }
            Stmt::Block(statements) => {
                let new_env = Environment::with_enclosing(self.env.clone());
                self.execute_block(statements, new_env, output)
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let v = &self.evaluate(condition)?;
                if self.is_truthy(v) {
                    self.execute(then_branch, output)
                } else if let Some(else_stmt) = else_branch {
                    self.execute(else_stmt, output)
                } else {
                    Ok(())
                }
            }
            Stmt::While { condition, body } => {
                loop {
                    let cond = self.evaluate(condition)?;

                    let v = self.is_truthy(&cond);
                    if !v {
                        break;
                    }

                    self.execute(body, output)?;
                }
                Ok(())
            }
            Stmt::Function { name, params, body } => {
                let func = LoxFunction {
                    name: name.lexeme.clone(),
                    params: params.iter().map(|t| t.lexeme.clone()).collect(),
                    body: body.clone(),
                    closure: self.env.clone(),
                };

                self.env
                    .borrow_mut()
                    .define(name.lexeme.clone(), Value::Function(Rc::new(func)));

                Ok(())
            }
            other => {
                eprintln!("⛔ Unexpected statement in execute(): {:?}", other);
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
            Value::Function(func) => format!("<fn {}>", func.name),
        }
    }

    fn execute_block<W: Write>(
        &mut self,
        statements: &[Stmt],
        new_env: Rc<RefCell<Environment>>,
        output: &mut W,
    ) -> Result<(), String> {
        let previous = self.env.clone();
        self.env = new_env;
        let result = (|| {
            for stmt in statements {
                self.execute(stmt, output)?;
            }
            Ok(())
        })();
        self.env = previous;
        result
    }

    pub fn execute_block_with_return(
        &mut self,
        statements: &[Stmt],
        env: Rc<RefCell<Environment>>,
    ) -> Result<(), ReturnValue> {
        let previous = self.env.clone();
        self.env = env;
        let result = (|| {
            for stmt in statements {
                match self.execute_with_return(stmt) {
                    Ok(()) => {}
                    Err(ret) => return Err(ret),
                }
            }
            Ok(())
        })();
        self.env = previous;
        result
    }

    fn execute_with_return(&mut self, stmt: &Stmt) -> Result<(), ReturnValue> {
        match stmt {
            Stmt::Return(expr_opt) => {
                let value = if let Some(expr) = expr_opt {
                    let v = self.evaluate(expr).map_err(ReturnValue::Runtime)?;
                    v
                } else {
                    Value::Nil
                };
                Err(ReturnValue::Return(value))
            }

            // важный момент: рекурсивно поддерживаем блоки, if, while, выражения и print
            Stmt::Block(statements) => {
                let new_env = Environment::with_enclosing(self.env.clone());
                self.execute_block_with_return(statements, new_env)
            }

            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_value = self.evaluate(condition).map_err(ReturnValue::Runtime)?;

                if self.is_truthy(&cond_value) {
                    self.execute_with_return(then_branch)
                } else if let Some(else_branch) = else_branch {
                    self.execute_with_return(else_branch)
                } else {
                    Ok(())
                }
            }

            Stmt::While { condition, body } => {
                let cond_value = self.evaluate(condition).map_err(ReturnValue::Runtime)?;

                while self.is_truthy(&cond_value) {
                    self.execute_with_return(body)?;
                }
                Ok(())
            }

            // другие делегируем
            other => {
                self.execute(other, &mut Vec::new()).map_err(ReturnValue::Runtime)
            }
        }
    }

}

#[derive(Default, Clone)]
pub struct Environment {
    values: HashMap<String, Value>,
    enclosing: Option<Rc<RefCell<Environment>>>,
}

// временно для Debug
impl std::fmt::Debug for Environment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Environment(...)")
    }
}

impl Environment {
    pub fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        self.values
            .get(name)
            .cloned()
            .or_else(|| self.enclosing.as_ref()?.borrow().get(name))
    }

    pub fn with_enclosing(enclosing: Rc<RefCell<Environment>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Environment {
            values: HashMap::new(),
            enclosing: Some(enclosing),
        }))
    }

    pub fn assign(&mut self, name: &str, value: Value) -> Option<()> {
        if self.values.contains_key(name) {
            self.values.insert(name.to_string(), value);
            Some(())
        } else if let Some(enclosing) = self.enclosing.as_mut() {
            enclosing.borrow_mut().assign(name, value)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;
    use crate::scanner::Scanner;

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

    #[test]
    fn test_if_else() {
        let output = run_and_capture("if (true) print 1; else print 2;");
        assert_eq!(output, vec!["1"]);

        let output = run_and_capture("if (false) print 1; else print 2;");
        assert_eq!(output, vec!["2"]);
    }

    #[test]
    fn test_variable_increment_and_use() {
        assert_eq!(
            run_and_capture("var x = 42; if (true) x = x + 2; print x; print x < 45;"),
            vec!["44", "true"]
        );
    }

    #[test]
    fn test_while_loop() {
        let output = run_and_capture(
            "var i = 0;
         while (i < 3) {
           print i;
           i = i + 1;
         }",
        );
        assert_eq!(output, vec!["0", "1", "2"]);
    }

    #[test]
    fn test_function_definition_and_call() {
        let output = run_and_capture(
            "
        fun add(a, b) {
            return a + b;
        }

        print add(2, 3);
        ",
        );
        assert_eq!(output, vec!["5"]);
    }

    #[test]
    fn test_function_empty_return() {
        let output = run_and_capture(
            "
        fun noop() {
            return;
        }

        print noop();
        ",
        );
        assert_eq!(output, vec!["nil"]);
    }

    #[test]
    fn test_early_return() {
        let output = run_and_capture(
            "
        fun test() {
            return 42;
            print \"unreachable\";
        }

        print test();
        ",
        );
        assert_eq!(output, vec!["42"]);
    }

    #[test]
    fn test_return_inside_if() {
        let output = run_and_capture(
            "
        fun check(flag) {
            if (flag) return 1;
            return 0;
        }

        print check(true);
        print check(false);
        ",
        );
        assert_eq!(output, vec!["1", "0"]);
    }

    #[test]
    fn test_nested_function_calls() {
        let output = run_and_capture(
            "
        fun square(x) {
            return x * x;
        }

        fun double(x) {
            return x + x;
        }

        print square(double(3)); // (3 + 3) * (3 + 3) = 36
        ",
        );
        assert_eq!(output, vec!["36"]);
    }
}
