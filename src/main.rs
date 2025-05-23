mod ast;
mod interpreter;
mod parser;
mod scanner;
mod token;

use interpreter::Interpreter;
use parser::Parser;
use scanner::Scanner;
use std::io::{self, Write};

fn main() {
    let stdin = io::stdin();
    let mut interpreter = Interpreter::new();

    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        let mut line = String::new();
        if stdin.read_line(&mut line).unwrap() == 0 {
            break;
        }

        let tokens = Scanner::new(&line).scan_tokens();
        let mut parser = Parser::new(tokens);
        let statements = parser.parse();

        if let Err(e) = interpreter.interpret_statements(&statements, &mut io::stdout()) {
            eprintln!("Runtime error: {e}");
        }
    }
}