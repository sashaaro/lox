mod token;
mod scanner;

use std::env;
use std::fs;
use std::io::{self, Write};
use crate::scanner::Scanner;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        writeln!(io::stderr(), "Usage: {} tokenize <filename>", args[0]).unwrap();
        return;
    }

    let command = &args[1];
    let filename = &args[2];

    match command.as_str() {
        "tokenize" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                writeln!(io::stderr(), "Failed to read file {}", filename).unwrap();
                String::new()
            });

            if !file_contents.is_empty() {
                let mut tokens = Scanner::new(&file_contents).scan_tokens();
                tokens.push(token::Token {
                    kind: token::TokenType::Eof,
                    lexeme: String::from(""),
                    literal: None,
                    line: 0
                });

                println!("EOF  null");
                // if has_error {
                //     std::process::exit(65);
                // }
            } else {
                println!("EOF  null"); // Placeholder, remove this line when implementing the scanner
            }
        }
        _ => {
            writeln!(io::stderr(), "Unknown command: {}", command).unwrap();
            return;
        }
    }
}
