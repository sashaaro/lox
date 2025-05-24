// expression     → equality ;
// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
// comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
// term           → factor ( ( "-" | "+" ) factor )* ;
// factor         → unary ( ( "/" | "*" ) unary )* ;
// unary          → ( "!" | "-" ) unary | primary ;
// primary        → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;

use crate::ast::{Expr, Literal, Stmt};
use crate::token::{Token, TokenType};

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    fn assignment(&mut self) -> Option<Expr> {
        let expr = self.or()?;

        if self.match_types(&[TokenType::Equal]) {
            let equals = self.previous().clone();
            let value = self.assignment()?; // правая часть

            if let Expr::Variable(name) = expr {
                return Some(Expr::Assign {
                    name,
                    value: Box::new(value),
                });
            }

            // невалидное lvalue
            eprintln!("Invalid assignment target at line {}", equals.line);
            return None;
        }

        Some(expr)
    }

    fn expression(&mut self) -> Option<Expr> {
        self.assignment()
    }

    fn or(&mut self) -> Option<Expr> {
        let mut expr = self.and()?;

        while self.match_types(&[TokenType::Or]) {
            let operator = self.previous().clone();
            let right = self.and()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Some(expr)
    }

    fn and(&mut self) -> Option<Expr> {
        let mut expr = self.equality()?;

        while self.match_types(&[TokenType::And]) {
            let operator = self.previous().clone();
            let right = self.equality()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Some(expr)
    }

    fn equality(&mut self) -> Option<Expr> {
        let mut expr = self.comparison()?;

        while self.match_types(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let operator = self.previous().clone();
            let right = self.comparison()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Some(expr)
    }

    fn comparison(&mut self) -> Option<Expr> {
        let mut expr = self.term()?;

        while self.match_types(&[
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let operator = self.previous().clone();
            let right = self.term()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Some(expr)
    }

    fn term(&mut self) -> Option<Expr> {
        let mut expr = self.factor()?;

        while self.match_types(&[TokenType::Minus, TokenType::Plus]) {
            let operator = self.previous().clone();
            let right = self.factor()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Some(expr)
    }

    fn factor(&mut self) -> Option<Expr> {
        let mut expr = self.unary()?;

        while self.match_types(&[TokenType::Slash, TokenType::Star]) {
            let operator = self.previous().clone();
            let right = self.unary()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Some(expr)
    }

    fn unary(&mut self) -> Option<Expr> {
        if self.match_types(&[TokenType::Bang, TokenType::Minus]) {
            let operator = self.previous().clone();
            let right = self.unary()?;
            return Some(Expr::Unary {
                operator,
                right: Box::new(right),
            });
        }

        self.call()
    }

    fn primary(&mut self) -> Option<Expr> {
        let token = self.advance();
        match &token.kind {
            TokenType::False => Some(Expr::Literal(Literal::Boolean(false))),
            TokenType::True => Some(Expr::Literal(Literal::Boolean(true))),
            TokenType::Nil => Some(Expr::Literal(Literal::Nil)),
            TokenType::Number => {
                if let Some(crate::token::Literal::Number(n)) = &token.literal {
                    Some(Expr::Literal(Literal::Number(*n)))
                } else {
                    None
                }
            }
            TokenType::String => {
                if let Some(crate::token::Literal::String(s)) = &token.literal {
                    Some(Expr::Literal(Literal::String(s.clone())))
                } else {
                    None
                }
            }
            TokenType::LeftParen => {
                let expr = self.expression()?;
                self.consume(TokenType::RightParen)?;
                Some(Expr::Grouping(Box::new(expr)))
            }
            TokenType::Identifier => Some(Expr::Variable(token.clone())),
            _ => None,
        }
    }

    fn match_types(&mut self, types: &[TokenType]) -> bool {
        for t in types {
            if self.check(t) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn consume(&mut self, token_type: TokenType) -> Option<Token> {
        if self.check(&token_type) {
            Some(self.advance())
        } else {
            None
        }
    }

    fn check(&self, token_type: &TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }
        &self.peek().kind == token_type
    }

    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous().clone()
    }

    fn is_at_end(&self) -> bool {
        self.peek().kind == TokenType::Eof
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    pub fn parse(&mut self) -> Vec<Stmt> {
        let mut statements = vec![];
        while !self.is_at_end() {
            if let Some(stmt) = self.declaration() {
                statements.push(stmt);
            }
        }
        statements
    }

    fn declaration(&mut self) -> Option<Stmt> {
        if self.match_types(&[TokenType::Fun]) {
            return self.function("function");
        }
        if self.match_types(&[TokenType::Var]) {
            return self.var_declaration();
        }
        self.statement()
    }

    fn statement(&mut self) -> Option<Stmt> {
        if self.match_types(&[TokenType::Print]) {
            self.print_statement()
        } else if self.match_types(&[TokenType::Return]) {
            self.return_statement()
        } else if self.match_types(&[TokenType::If]) {
            self.if_statement()
        } else if self.match_types(&[TokenType::While]) {
            self.while_statement()
        } else if self.match_types(&[TokenType::LeftBrace]) {
            Some(Stmt::Block(self.block()?))
        } else {
            self.expression_statement()
        }
    }

    fn return_statement(&mut self) -> Option<Stmt> {
        let keyword = self.previous(); // только если ты хочешь трекать место return
        let value = if !self.check(&TokenType::Semicolon) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(TokenType::Semicolon)?;
        Some(Stmt::Return(value))
    }

    fn print_statement(&mut self) -> Option<Stmt> {
        let value = self.expression()?;
        self.consume(TokenType::Semicolon)?;
        Some(Stmt::Print(value))
    }

    fn expression_statement(&mut self) -> Option<Stmt> {
        let expr = self.expression()?;
        self.consume(TokenType::Semicolon)?;
        Some(Stmt::Expression(expr))
    }

    fn var_declaration(&mut self) -> Option<Stmt> {
        let name = self.consume(TokenType::Identifier)?;

        let initializer = if self.match_types(&[TokenType::Equal]) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(TokenType::Semicolon)?;
        Some(Stmt::Var { name, initializer })
    }

    fn block(&mut self) -> Option<Vec<Stmt>> {
        let mut statements = vec![];

        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            if let Some(stmt) = self.declaration() {
                statements.push(stmt);
            }
        }

        self.consume(TokenType::RightBrace)?;
        Some(statements)
    }

    fn if_statement(&mut self) -> Option<Stmt> {
        self.consume(TokenType::LeftParen)?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen)?;

        let then_branch = Box::new(self.statement()?);
        let else_branch = if self.match_types(&[TokenType::Else]) {
            Some(Box::new(self.statement()?))
        } else {
            None
        };

        Some(Stmt::If {
            condition,
            then_branch,
            else_branch,
        })
    }

    fn while_statement(&mut self) -> Option<Stmt> {
        self.consume(TokenType::LeftParen)?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen)?;
        let body = Box::new(self.statement()?);
        Some(Stmt::While { condition, body })
    }

    fn function(&mut self, _kind: &str) -> Option<Stmt> {
        let name = self.consume(TokenType::Identifier)?;
        self.consume(TokenType::LeftParen)?;
        let mut params = vec![];

        if !self.check(&TokenType::RightParen) {
            loop {
                if params.len() >= 255 {
                    eprintln!("Can't have more than 255 parameters.");
                    return None;
                }
                params.push(self.consume(TokenType::Identifier)?);
                if !self.match_types(&[TokenType::Comma]) {
                    break;
                }
            }
        }

        self.consume(TokenType::RightParen)?;
        self.consume(TokenType::LeftBrace)?;
        let body = self.block()?;
        Some(Stmt::Function { name, params, body })
    }

    fn finish_call(&mut self, callee: Expr) -> Option<Expr> {
        let mut arguments = vec![];

        if !self.check(&TokenType::RightParen) {
            loop {
                if arguments.len() >= 255 {
                    eprintln!("Can't have more than 255 arguments.");
                    return None;
                }
                arguments.push(self.expression()?);
                if !self.match_types(&[TokenType::Comma]) {
                    break;
                }
            }
        }

        let paren = self.consume(TokenType::RightParen)?;
        Some(Expr::Call {
            callee: Box::new(callee),
            paren,
            arguments,
        })
    }

    fn call(&mut self) -> Option<Expr> {
        let mut expr = self.primary()?;
        while self.match_types(&[TokenType::LeftParen]) {
            expr = self.finish_call(expr)?;
        }
        Some(expr)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Expr::Assign;
    use crate::ast::{Expr, Literal, Stmt};
    use crate::scanner::Scanner;
    use crate::token::TokenType;

    fn parse_stmt(source: &str) -> Stmt {
        let tokens = Scanner::new(source).scan_tokens();
        let mut parser = Parser::new(tokens);
        let stmts = parser.parse();
        assert_eq!(stmts.len(), 1, "Expected one statement");
        stmts.into_iter().next().unwrap()
    }

    #[test]
    fn test_parse_literal_expression_statement() {
        let stmt = parse_stmt("42;");
        match stmt {
            Stmt::Expression(Expr::Literal(Literal::Number(n))) => assert_eq!(n, 42.0),
            _ => panic!("Expected expression statement with number literal"),
        }
    }

    #[test]
    fn test_parse_print_statement() {
        let stmt = parse_stmt("print 123;");
        match stmt {
            Stmt::Print(Expr::Literal(Literal::Number(n))) => assert_eq!(n, 123.0),
            _ => panic!("Expected print statement with number literal"),
        }
    }

    #[test]
    fn test_parse_var_declaration() {
        let stmt = parse_stmt("var x = 5;");
        match stmt {
            Stmt::Var {
                name,
                initializer: Some(Expr::Literal(Literal::Number(n))),
            } => {
                assert_eq!(name.lexeme, "x");
                assert_eq!(n, 5.0);
            }
            _ => panic!("Expected variable declaration with initializer"),
        };
    }

    #[test]
    fn test_parse_expression() {
        let stmt = parse_stmt("x = x + 5;");
        match stmt {
            Stmt::Expression(Assign { name, value }) => {
                assert_eq!(name.lexeme, "x");
                match *value {
                    Expr::Binary {
                        left,
                        operator,
                        right,
                    } => {
                        // left: Variable("x")
                        match *left {
                            Expr::Variable(token) => {
                                assert_eq!(token.lexeme, "x");
                            }
                            _ => panic!("Expected left side to be a variable"),
                        }

                        // operator: +
                        assert_eq!(operator.kind, TokenType::Plus);

                        // right: Literal(Number(5.0))
                        match *right {
                            Expr::Literal(Literal::Number(n)) => {
                                assert_eq!(n, 5.0);
                            }
                            _ => panic!("Expected right side to be a number literal"),
                        }
                    } // допустим базовый разбор бинарного выражения
                    _ => panic!("Expected binary expression as assignment value"),
                }
            }
            _ => panic!("Expected expression with initializer"),
        }
    }

    #[test]
    fn test_parse_binary_expression() {
        let stmt = parse_stmt("1 + 2 * 3 == 7;");
        match stmt {
            Stmt::Expression(Expr::Binary { operator, .. }) => {
                assert_eq!(operator.kind, TokenType::EqualEqual);
            }
            _ => panic!("Expected binary expression statement"),
        }
    }
}
