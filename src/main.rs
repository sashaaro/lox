use std::env;
use std::fs;
use std::io::{self, Write};

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
                let file_contents_chars = file_contents.chars();
                for c in file_contents_chars {
                    match c {
                        '(' => println!("LEFT_PAREN ( null"),
                        ')' => println!("RIGHT_PAREN ) null"),
                        '{' => println!("LEFT_BRACE {{ null"),
                        '}' => println!("RIGHT_BRACE }} null"),
                        ',' => println!("COMMA , null"),
                        '.' => println!("DOT . null"),
                        '-' => println!("MINUS - null"),
                        '+' => println!("PLUS + null"),
                        ';' => println!("SEMICOLON ; null"),
                        '*' => println!("STAR * null"),
                        '!' => println!("BANG ! null"),
                        '=' => println!("EQUAL = null"),
                        '<' => println!("LESS < null"),
                        '>' => println!("GREATER > null"),
                        '/' => println!("SLASH / null"),
                        '"' => println!("DOUBLE_QUOTE \" null"),
                        ' ' => println!("SPACE  null"),
                        '\t' => println!("TAB  null"),
                        '\n' => println!("NEWLINE  null"),
                        _ => {}
                    }
                }
                println!("EOF  null");
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
