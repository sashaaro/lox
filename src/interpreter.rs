use crate::ast::{Expr, Literal, Stmt};
use crate::core::ClockNative;
use crate::token::TokenType;
use std::cell::RefCell;
use std::collections::HashMap;
use std::io::Write;
use std::rc::Rc;

#[derive(Clone)]
pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
    Function(Rc<LoxFunction>),
    NativeFunction(Rc<dyn LoxCallable>),
    Array(Rc<RefCell<Vec<Value>>>),
    Class(Rc<LoxClass>),
    Instance(Rc<RefCell<LoxInstance>>),
}

impl Value {
    pub fn as_instance(&self) -> Result<Rc<RefCell<LoxInstance>>, String> {
        if let Value::Instance(inst) = self {
            Ok(Rc::clone(inst))
        } else {
            Err("Expected instance.".into())
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        use Value::*;
        match (self, other) {
            (Nil, Nil) => true,
            (Boolean(a), Boolean(b)) => a == b,
            (Number(a), Number(b)) => a == b,
            (String(a), String(b)) => a == b,
            // нельзя сравнивать функции
            _ => false,
        }
    }
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "Number({})", n),
            Value::String(s) => write!(f, "String({:?})", s),
            Value::Boolean(b) => write!(f, "Boolean({})", b),
            Value::Nil => write!(f, "Nil"),
            Value::Function(func) => write!(f, "<fn {}>", func.name),
            Value::NativeFunction(native) => write!(f, "<native fn {}>", native.name()),
            Value::Array(arr) => write!(f, "Array({:?})", arr),
            Value::Class(class) => write!(f, "Class({})", class.name),
            Value::Instance(instance) => write!(f, "Instance({:?})", instance.borrow().class)
        }
    }
}

#[derive(Debug)]
pub enum ReturnValue {
    Return(Value),
    Runtime(String),
}

pub trait LoxCallable {
    fn arity(&self) -> usize;
    fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>) -> Result<Value, String>;
    fn name(&self) -> &str;
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

impl LoxCallable for LoxFunction {
    fn arity(&self) -> usize {
        self.params.len()
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>) -> Result<Value, String> {
        let env = Environment::with_enclosing(self.closure.clone());

        for (param, arg) in self.params.iter().zip(args.into_iter()) {
            eprintln!("binding param: {} = {:?}", param, arg);
            
            env.borrow_mut().define(param.clone(), arg);
        }

        match interpreter.execute_block_with_return(&self.body, env) {
            Ok(_) => Ok(Value::Nil),
            Err(ReturnValue::Return(val)) => Ok(val),
            Err(ReturnValue::Runtime(e)) => Err(e),
        }
    }
}

impl LoxFunction {
    pub fn bind(&self, instance: Rc<RefCell<LoxInstance>>) -> Rc<LoxFunction> {
        let env = Environment::with_enclosing(self.closure.clone());
        env.borrow_mut().define("this".to_string(), Value::Instance(instance));
        Rc::new(LoxFunction {
            name: self.name.clone(),
            params: self.params.clone(),
            body: self.body.clone(),
            closure: env,
        })
    }
}

#[derive(Debug, Clone)]
pub struct LoxClass {
    pub name: String,
    pub methods: HashMap<String, Rc<LoxFunction>>,
}

impl LoxCallable for LoxClass {
    fn arity(&self) -> usize {
        if let Some(init) = self.methods.get("init") {
            init.arity()
        } else {
            0
        }
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>) -> Result<Value, String> {
        let instance = Rc::new(RefCell::new(LoxInstance {
            class: Rc::new(self.clone()),
            fields: HashMap::new(),
        }));

        // вызов init() если есть
        if let Some(init) = self.methods.get("init") {
            let bound = init.bind(instance.clone());
            bound.call(interpreter, args)?;
        }

        Ok(Value::Instance(instance))
    }
}

#[derive(Debug)]
pub struct LoxInstance {
    pub class: Rc<LoxClass>,
    pub fields: HashMap<String, Value>,
}

pub struct Interpreter {
    env: Rc<RefCell<Environment>>,
}

impl Interpreter {
    pub fn new() -> Self {
        let env = Rc::new(RefCell::new(Environment::default()));

        env.borrow_mut()
            .define("clock".into(), Value::NativeFunction(Rc::new(ClockNative)));

        Interpreter { env }
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
                    .ok_or_else(|| {
                        format!("Undefined variable '{}'", name)
                    })
            }
            Expr::Binary {
                left,
                operator,
                right,
            } if matches!(operator.kind, TokenType::Or | TokenType::And) => {
                let left_val = self.evaluate(left)?;

                match operator.kind {
                    TokenType::Or => {
                        if self.is_truthy(&left_val) {
                            return Ok(left_val);
                        }
                    }
                    TokenType::And => {
                        if !self.is_truthy(&left_val) {
                            return Ok(left_val);
                        }
                    }
                    _ => {}
                }

                self.evaluate(right)
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
            }
            Expr::Call {
                callee, arguments, ..
            } => {
                let callee_val = self.evaluate(callee)?;
                let args = arguments
                    .iter()
                    .map(|arg| self.evaluate(arg))
                    .collect::<Result<Vec<_>, _>>()?;

                match callee_val {
                    Value::Function(f) => f.call(self, args),
                    Value::NativeFunction(f) => f.call(self, args),
                    Value::Class(c) => c.call(self, args),
                    _ => Err("Can only call functions.".into()),
                }
            }
            Expr::Array(items) => {
                let values = items
                    .iter()
                    .map(|e| self.evaluate(e))
                    .collect::<Result<_, _>>()?;
                Ok(Value::Array(Rc::new(RefCell::new(values))))
            }
            Expr::Index { object, index } => {
                let object_val = self.evaluate(object)?;
                let index_val = self.evaluate(index)?;

                match (&object_val, &index_val) {
                    (Value::Array(arr), Value::Number(n)) => {
                        let idx = *n as usize;
                        let arr_ref = arr.borrow();
                        if idx < arr_ref.len() {
                            Ok(arr_ref[idx].clone())
                        } else {
                            Err("Index out of bounds".into())
                        }
                    }
                    _ => Err("Only arrays can be indexed with numbers".into()),
                }
            }
            Expr::SetIndex {
                object,
                index,
                value,
            } => {
                let object_val = self.evaluate(object)?;
                let index_val = self.evaluate(index)?;
                let value_val = self.evaluate(value)?;

                match (object_val, index_val) {
                    (Value::Array(arr), Value::Number(n)) => {
                        let idx = n as usize;
                        let mut arr_ref = arr.borrow_mut();
                        if idx < arr_ref.len() {
                            arr_ref[idx] = value_val.clone();
                            Ok(value_val)
                        } else {
                            Err("Index out of bounds".into())
                        }
                    }
                    _ => Err("Can only assign into array[index]".into()),
                }
            },
            Expr::Get { object, name } => {
                let object_val = self.evaluate(object)?;

                if let Value::Instance(inst) = &object_val {
                    let inst = inst.borrow();

                    if let Some(val) = inst.fields.get(&name.lexeme) {
                        return Ok(val.clone());
                    }

                    if let Some(method) = inst.class.methods.get(&name.lexeme) {
                        return Ok(Value::Function(method.bind(Rc::clone(&object_val.as_instance()?))));
                    }

                    return Err(format!("Undefined property '{}'", name.lexeme));
                }

                Err("Only instances have properties.".into())
            },
            Expr::Set { object, name, value } => {
                eprintln!("🧩 SET: object = {:?}", 1);

                let object_val = self.evaluate(object)?;
                let value_val = self.evaluate(value)?;

                if let Value::Instance(inst) = object_val {
                    inst.borrow_mut().fields.insert(name.lexeme.clone(), value_val.clone());
                    Ok(value_val)
                } else {
                    Err("Only instances have fields.".into())
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

    fn execute_stmt(&mut self, stmt: &Stmt, output: &mut impl Write) -> Result<(), String> {
        match stmt {
            Stmt::Print(expr) => {
                let val = self.evaluate(expr)?;
                writeln!(output, "{}", self.stringify(&val)).map_err(|e| e.to_string())
            }
            Stmt::Expression(expr) => {
                self.evaluate(expr)?;
                Ok(())
            }
            Stmt::Var { name, initializer } => {
                let val = if let Some(expr) = initializer {
                    self.evaluate(expr)?
                } else {
                    Value::Nil
                };
                self.env.borrow_mut().define(name.lexeme.clone(), val);
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
                let cond_val = self.evaluate(condition)?;
                if self.is_truthy(&cond_val) {
                    self.execute_stmt(then_branch, output)
                } else if let Some(else_branch) = else_branch {
                    self.execute_stmt(else_branch, output)
                } else {
                    Ok(())
                }
            }
            Stmt::While { condition, body } => {
                while {
                    let v = &self.evaluate(condition)?;
                    self.is_truthy(v)
                } {
                    self.execute_stmt(body, output)?;
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
            Stmt::Return(_) => {
                // только для execute_with_return
                Err("Unexpected return statement outside of function.".into())
            }
            Stmt::Class { name, methods } => {
                let mut method_map = HashMap::new();

                for method in methods {
                    if let Stmt::Function { name: method_name, params, body } = method {
                        let func = LoxFunction {
                            name: method_name.lexeme.clone(),
                            params: params.iter().map(|t| t.lexeme.clone()).collect(),
                            body: body.clone(),
                            closure: self.env.clone(),
                        };
                        method_map.insert(method_name.lexeme.clone(), Rc::new(func));
                    }
                }

                let class = LoxClass {
                    name: name.lexeme.clone(),
                    methods: method_map,
                };

                self.env.borrow_mut().define(name.lexeme.clone(), Value::Class(Rc::new(class)));
                Ok(())
            }
        }
    }

    fn execute<W: Write>(&mut self, stmt: &Stmt, output: &mut W) -> Result<(), String> {
        self.execute_stmt_internal(stmt, output, false)
            .map_err(|e| match e {
                ReturnValue::Runtime(e) => e,
                ReturnValue::Return(_) => "Return outside of function.".into(),
            })
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
            Value::NativeFunction(native) => format!("<native fn {}>", native.name()),
            Value::Array(items) => {
                let s = items
                    .borrow()
                    .iter()
                    .map(|v| self.stringify(v))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("[{}]", s)
            },
            Value::Class(class) => format!("<class {}>", class.name),
            Value::Instance(instance) => format!("<instance {}>", instance.borrow().class.name)
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
        self.execute_stmt_internal(stmt, &mut Vec::new(), true)
    }

    fn execute_stmt_internal(
        &mut self,
        stmt: &Stmt,
        output: &mut impl Write,
        allow_return: bool,
    ) -> Result<(), ReturnValue> {
        match stmt {
            Stmt::Return(expr_opt) => {
                if !allow_return {
                    return Err(ReturnValue::Runtime(
                        "Can't return from top-level code.".into(),
                    ));
                }
                let value = if let Some(expr) = expr_opt {
                    self.evaluate(expr).map_err(ReturnValue::Runtime)?
                } else {
                    Value::Nil
                };
                Err(ReturnValue::Return(value))
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond = self.evaluate(condition).map_err(ReturnValue::Runtime)?;
                if self.is_truthy(&cond) {
                    self.execute_stmt_internal(then_branch, output, allow_return)
                } else if let Some(else_stmt) = else_branch {
                    self.execute_stmt_internal(else_stmt, output, allow_return)
                } else {
                    Ok(())
                }
            }
            Stmt::While { condition, body } => {
                while {
                    let v = &self.evaluate(condition).map_err(ReturnValue::Runtime)?;
                    self.is_truthy(v)
                } {
                    self.execute_stmt_internal(body, output, allow_return)?;
                }
                Ok(())
            }
            Stmt::Block(statements) => {
                let new_env = Environment::with_enclosing(self.env.clone());
                let previous = self.env.clone();
                self.env = new_env;
                let result = (|| {
                    for stmt in statements {
                        self.execute_stmt_internal(stmt, output, allow_return)?;
                    }
                    Ok(())
                })();
                self.env = previous;
                result
            }
            Stmt::Class { name, methods } => {
                let mut method_map = HashMap::new();

                for method in methods {
                    if let Stmt::Function { name: method_name, params, body } = method {
                        let func = LoxFunction {
                            name: method_name.lexeme.clone(),
                            params: params.iter().map(|t| t.lexeme.clone()).collect(),
                            body: body.clone(),
                            closure: self.env.clone(), // замыкание
                        };
                        method_map.insert(method_name.lexeme.clone(), Rc::new(func));
                    }
                }

                let class = LoxClass {
                    name: name.lexeme.clone(),
                    methods: method_map,
                };

                self.env.borrow_mut().define(name.lexeme.clone(), Value::Class(Rc::new(class)));
                Ok(())
            }
            // обычные без return
            _ => self
                .execute_stmt(stmt, output)
                .map_err(ReturnValue::Runtime),
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

    #[test]
    fn test_logical_and_or() {
        let result = run_and_capture("print true or false;");
        assert_eq!(result, vec!["true"]);

        let result = run_and_capture("print false or true;");
        assert_eq!(result, vec!["true"]);

        let result = run_and_capture("print false and true;");
        assert_eq!(result, vec!["false"]);

        let result = run_and_capture("print true and false;");
        assert_eq!(result, vec!["false"]);
    }

    #[test]
    fn test_clock_function() {
        let result = run_and_capture("print clock();");
        assert_eq!(result.len(), 1);
        let n = result[0].parse::<f64>().unwrap();
        assert!(n > 0.0);
    }

    #[test]
    fn test_array_literal_and_indexing() {
        let output = run_and_capture(
            "
        var a = [1, 2, 3];
        print a[0];
        print a[2];
        ",
        );
        assert_eq!(output, vec!["1", "3"]);
    }

    #[test]
    fn test_array_index_assignment() {
        let result = run_and_capture(
            "
        var a = [10, 20, 30];
        a[1] = 42;
        print a[1];
        ",
        );
        assert_eq!(result, vec!["42"]);
    }

    #[test]
    fn test_closure_counter() {
        let output = run_and_capture(
            "
        fun makeCounter() {
            var count = 0;
            fun inc() {
                count = count + 1;
                return count;
            }
            return inc;
        }

        var c = makeCounter();
        print c(); // 1
        print c(); // 2
        ",
        );
        assert_eq!(output, vec!["1", "2"]);
    }

    #[test]
    fn test_class_instance_init() {
        let output = run_and_capture(
            "
        class Dog {
            init(name) {
                this.name = name;
            }

            speak() {
                print this.name + \" says woof\";
            }
        }

        var d = Dog(\"Rex\");
        d.speak();
        ",
        );
        assert_eq!(output, vec!["Rex says woof"]);
    }
}
