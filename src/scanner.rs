use crate::token::{Literal, Token, TokenType};

pub struct Scanner<'a> {
    source: &'a str,
    chars: std::str::Chars<'a>,
    start: usize,
    current: usize,
    line: usize,
    tokens: Vec<Token>,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Scanner {
            source,
            chars: source.chars(),
            start: 0,
            current: 0,
            line: 1,
            tokens: Vec::new(),
        }
    }

    pub fn scan_tokens(mut self) -> Vec<Token> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        self.tokens.push(Token {
            kind: TokenType::Eof,
            lexeme: "".to_string(),
            literal: None,
            line: self.line,
        });

        self.tokens
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn scan_token(&mut self) {
        let c = self.advance();

        match c {
            '(' => self.add_token(TokenType::LeftParen),
            ')' => self.add_token(TokenType::RightParen),
            '{' => self.add_token(TokenType::LeftBrace),
            '}' => self.add_token(TokenType::RightBrace),
            '[' => self.add_token(TokenType::LeftBracket),
            ']' => self.add_token(TokenType::RightBracket),
            ',' => self.add_token(TokenType::Comma),
            '.' => self.add_token(TokenType::Dot),
            '-' => self.add_token(TokenType::Minus),
            '+' => self.add_token(TokenType::Plus),
            ';' => self.add_token(TokenType::Semicolon),
            '*' => self.add_token(TokenType::Star),
            '!' => {
                let kind = if self.match_char('=') {
                    TokenType::BangEqual
                } else {
                    TokenType::Bang
                };
                self.add_token(kind);
            }
            '=' => {
                let kind = if self.match_char('=') {
                    TokenType::EqualEqual
                } else {
                    TokenType::Equal
                };
                self.add_token(kind);
            }
            '<' => {
                let kind = if self.match_char('=') {
                    TokenType::LessEqual
                } else {
                    TokenType::Less
                };
                self.add_token(kind);
            }
            '>' => {
                let kind = if self.match_char('=') {
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                };
                self.add_token(kind);
            }
            '/' => self.add_token(TokenType::Slash),
            '0'..='9' => self.number(),
            'a'..='z' | 'A'..='Z' | '_' => self.identifier(),
            '"' => self.string(),
            ' ' | '\r' | '\t' => {} // игнорируем whitespace
            '\n' => self.line += 1,
            _ => {
                // позже обработаем числа, строки и идентификаторы
                println!("Unexpected character: {}", c);
            }
        }
    }

    fn advance(&mut self) -> char {
        let c = self.chars.next().unwrap();
        self.current += c.len_utf8();
        c
    }

    fn add_token(&mut self, kind: TokenType) {
        let lexeme = &self.source[self.start..self.current];
        self.tokens.push(Token {
            kind,
            lexeme: lexeme.to_string(),
            literal: None,
            line: self.line,
        });
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        let next_char = self.source[self.current..].chars().next().unwrap();
        if next_char != expected {
            return false;
        }

        // если совпало — съедаем
        self.advance();
        true
    }

    fn peek(&self) -> char {
        self.source[self.current..].chars().next().unwrap_or('\0')
    }

    fn peek_next(&self) -> char {
        let mut chars = self.source[self.current..].chars();
        chars.next();
        chars.next().unwrap_or('\0')
    }

    fn number(&mut self) {
        while self.peek().is_ascii_digit() {
            self.advance();
        }

        // Проверяем десятичную точку
        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            self.advance(); // точка
            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }

        let lexeme = &self.source[self.start..self.current];
        let value = lexeme.parse::<f64>().unwrap(); // safe: мы сами парсили
        self.add_token_with_literal(TokenType::Number, Some(Literal::Number(value)));
    }

    fn add_token_with_literal(&mut self, kind: TokenType, literal: Option<Literal>) {
        let lexeme = &self.source[self.start..self.current];
        self.tokens.push(Token {
            kind,
            lexeme: lexeme.to_string(),
            literal,
            line: self.line,
        });
    }

    fn string(&mut self) {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            println!("Unterminated string on line {}", self.line);
            return;
        }

        // Закрывающая кавычка
        self.advance();

        // Убираем кавычки по краям
        let value = &self.source[self.start + 1..self.current - 1];
        self.add_token_with_literal(TokenType::String, Some(Literal::String(value.to_string())));
    }

    fn identifier(&mut self) {
        while self.peek().is_ascii_alphanumeric() || self.peek() == '_' {
            self.advance();
        }

        let text = &self.source[self.start..self.current];
        let kind = identifier_type(text);
        self.add_token(kind);
    }
}

fn identifier_type(text: &str) -> TokenType {
    match text {
        "and" => TokenType::And,
        "class" => TokenType::Class,
        "else" => TokenType::Else,
        "false" => TokenType::False,
        "for" => TokenType::For,
        "fun" => TokenType::Fun,
        "if" => TokenType::If,
        "nil" => TokenType::Nil,
        "or" => TokenType::Or,
        "print" => TokenType::Print,
        "return" => TokenType::Return,
        "super" => TokenType::Super,
        "this" => TokenType::This,
        "true" => TokenType::True,
        "var" => TokenType::Var,
        "while" => TokenType::While,
        _ => TokenType::Identifier,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::TokenType::*;

    fn scan(source: &str) -> Vec<TokenType> {
        Scanner::new(source)
            .scan_tokens()
            .into_iter()
            .map(|t| t.kind)
            .collect()
    }

    #[test]
    fn test_single_char_tokens() {
        let result = scan("(){},.+-*/;");
        assert_eq!(
            result,
            vec![
                LeftParen, RightParen, LeftBrace, RightBrace, Comma, Dot, Plus, Minus, Star, Slash,
                Semicolon, Eof
            ]
        );
    }

    #[test]
    fn test_ignores_whitespace() {
        let result = scan(" \t\n() ");
        assert_eq!(result, vec![LeftParen, RightParen, Eof]);
    }

    #[test]
    fn test_unexpected_char() {
        let result = scan("@");
        assert_eq!(result, vec![Eof]); // мы не добавляем токены на неожиданные символы пока
                                       // можно позже улучшить: добавить список ошибок
    }

    #[test]
    fn test_two_char_tokens() {
        let result = scan("! != = == < <= > >=");
        assert_eq!(
            result,
            vec![
                Bang,
                BangEqual,
                Equal,
                EqualEqual,
                Less,
                LessEqual,
                Greater,
                GreaterEqual,
                Eof
            ]
        );
    }

    #[test]
    fn test_numbers() {
        let result = Scanner::new("42 3.14").scan_tokens();

        assert_eq!(result[0].kind, Number);
        assert_eq!(result[1].kind, Number);
        assert_eq!(result[2].kind, Eof);

        match &result[0].literal {
            Some(Literal::Number(n)) => assert_eq!(*n, 42.0),
            _ => panic!("Expected number literal"),
        }

        match &result[1].literal {
            Some(Literal::Number(n)) => assert_eq!(*n, 3.14),
            _ => panic!("Expected number literal"),
        }
    }

    #[test]
    fn test_string_literal() {
        let tokens = Scanner::new("\"hello world\"").scan_tokens();

        assert_eq!(tokens[0].kind, String);
        match &tokens[0].literal {
            Some(Literal::String(s)) => assert_eq!(s, "hello world"),
            _ => panic!("Expected string literal"),
        }

        assert_eq!(tokens[1].kind, Eof);
    }

    #[test]
    fn test_unterminated_string() {
        let tokens = Scanner::new("\"hello").scan_tokens();

        // Не должен паниковать. Просто будет только Eof, без строки.
        assert_eq!(tokens.last().unwrap().kind, Eof);
    }

    #[test]
    fn test_identifier_and_keywords() {
        let result = Scanner::new("var foo = true;").scan_tokens();

        use TokenType::*;
        let kinds: Vec<_> = result.iter().map(|t| &t.kind).collect();

        assert_eq!(
            kinds,
            vec![&Var, &Identifier, &Equal, &True, &Semicolon, &Eof]
        );

        assert_eq!(result[1].lexeme, "foo");
    }
}
