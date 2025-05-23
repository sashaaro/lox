mod token;
mod scanner;
mod ast;
mod parser;
mod interpreter;

use scanner::Scanner;

fn main() {
    let source = "(){},.+-;";
    let scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens();

    for token in tokens {
        println!("{:?}", token);
    }
}