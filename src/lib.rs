pub mod ast;
pub mod core;
pub mod interpreter;
pub mod parser;
pub mod scanner;
pub mod token;

pub use interpreter::{Interpreter, LoxCallable, Value};
pub use parser::Parser;
pub use scanner::Scanner;

/// Вычислить guard-скрипт на lox и вернуть «истинность» его последнего выражения.
///
/// `setup` инжектирует контекст в окружение интерпретатора перед запуском —
/// например переменные `uri`/`method` и native-функцию `header(name)`.
/// Вывод (`print`) отбрасывается: для guard-ов важен только результат.
pub fn eval_guard(
    source: &str,
    setup: impl FnOnce(&mut Interpreter),
) -> Result<bool, String> {
    let tokens = Scanner::new(source).scan_tokens();
    let statements = Parser::new(tokens).parse();

    let mut interp = Interpreter::new();
    setup(&mut interp);

    let mut sink = std::io::sink();
    interp.interpret_statements(&statements, &mut sink)?;
    Ok(interp.last_truthy())
}

#[cfg(test)]
mod guard_tests {
    use super::*;

    #[test]
    fn guard_uses_injected_variable() {
        let ok = eval_guard("uri == \"/admin\";", |i| {
            i.define_global("uri", Value::String("/admin".into()));
        })
        .unwrap();
        assert!(ok);

        let denied = eval_guard("uri == \"/admin\";", |i| {
            i.define_global("uri", Value::String("/public".into()));
        })
        .unwrap();
        assert!(!denied);
    }

    #[test]
    fn guard_multi_statement() {
        let ok = eval_guard(
            "var allowed = method == \"GET\"; allowed and uri == \"/\";",
            |i| {
                i.define_global("method", Value::String("GET".into()));
                i.define_global("uri", Value::String("/".into()));
            },
        )
        .unwrap();
        assert!(ok);
    }
}
