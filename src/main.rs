use std::env;
use std::fs;
use std::io::{self, Write};
use std::str::Chars;

#[derive(Debug)]
enum Token {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,
    BANG,
    BangEqual,
    EQUAL,
    EqualEqual,
    GREATER,
    GreaterEqual,
    LESS,
    LessEqual,
    IDENTIFIER,
    DoubleQuote,
    SPACE,
    TAB,
    NEWLINE,

    // Literals
    STRING(String),
    NUMBER(f64),

    ERROR(String),
}

struct Tokenizer<'a> {
    source_code: Chars<'a>,
    line: usize,
    column: usize,
}

impl<'a> Tokenizer<'a> {
    fn new(source_code: Chars) -> Tokenizer {
        Tokenizer {
            source_code,
            line: 1,
            column: 1,
        }
    }

    fn scan_tokens(&self) -> Vec<(char, Token)> {
        let mut r = vec![];
        for (i, c) in self.source_code.clone().into_iter().enumerate() {
            let token: Token = match c {
                '(' => Token::LeftParen,
                ')' => Token::RightParen,
                '{' => Token::LeftBrace,
                '}' => Token::RightBrace,
                ',' => Token::COMMA,
                '.' => Token::DOT,
                '-' => Token::MINUS,
                '+' => Token::PLUS,
                ';' => Token::SEMICOLON,
                '*' => Token::STAR,
                '!' => Token::BANG,
                '=' => Token::EQUAL,
                '<' => Token::LESS,
                '>' => Token::GREATER,
                '/' => Token::SLASH,
                '"' => Token::DoubleQuote,
                ' ' => Token::SPACE,
                '\t' => Token::TAB,
                '\n' => Token::NEWLINE,
                _ => Token::ERROR(format!(
                    // '$' | '#'
                    "[Line {}] Error: Unexpected character: {}",
                    self.line, c
                )),
            };
            r.push((c, token));
        }

        r
    }
}

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
                let chars = file_contents.chars();
                let mut tokens = Tokenizer::new(chars).scan_tokens();

                tokens.sort_by_key(|t| match t.1 {
                    Token::ERROR(_) => 0,
                    _ => 1,
                });

                let has_error = tokens.iter().any(|t| match t.1 {
                    Token::ERROR(_) => true,
                    _ => false,
                });

                for token in tokens {
                    match token.1 {
                        Token::ERROR(err) => eprintln!("{}", err),
                        _ => println!("{:?} {} null", token.1, token.0),
                    }
                }

                println!("EOF  null");
                if has_error {
                    std::process::exit(65);
                }
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
