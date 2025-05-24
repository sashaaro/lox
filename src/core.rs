use crate::interpreter::{Interpreter, LoxCallable, Value};

pub struct ClockNative;

impl LoxCallable for ClockNative {
    fn arity(&self) -> usize {
        0
    }

    fn name(&self) -> &str {
        "clock"
    }

    fn call(&self, _interpreter: &mut Interpreter, _args: Vec<Value>) -> Result<Value, String> {
        use std::time::{SystemTime, UNIX_EPOCH};
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs_f64();
        Ok(Value::Number(now))
    }
}
