use crate::ast::{Expr, Op, Program, Stmt};
use crate::lexer::{lex_line, Token};
use crate::util::die;

pub fn parse_program(source: &str) -> Program {
    let mut stmts = Vec::new();

    for (line_no, line) in source.lines().enumerate() {
        let trimmed = line.trim();
        if trimmed.is_empty() || trimmed.starts_with('#') {
            continue;
        }

        let tokens = lex_line(trimmed, line_no);
        if tokens.is_empty() {
            continue;
        }
        let mut parser = Parser::new(tokens, line_no);
        let stmt = parser.parse_stmt();
        stmts.push(stmt);
    }

    Program { stmts }
}

struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    line_no: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>, line_no: usize) -> Self {
        Self {
            tokens,
            pos: 0,
            line_no,
        }
    }

    fn parse_stmt(&mut self) -> Stmt {
        match self.peek() {
            Some(Token::Ident(kw)) if kw == "print" => {
                self.next();
                let exprs = self.parse_expr_list();
                self.expect_end();
                Stmt::Print(exprs)
            }
            Some(Token::Ident(kw)) if kw == "let" => {
                self.next();
                let name = match self.next() {
                    Some(Token::Ident(s)) => s,
                    _ => self.error("Expected identifier after 'let'"),
                };
                self.expect(Token::Eq, "Expected '=' after variable name");
                let expr = self.parse_expr();
                self.expect_end();
                Stmt::Let { name, expr }
            }
            _ => self.error("Unknown statement. Use 'let' or 'print'."),
        }
    }

    fn parse_expr(&mut self) -> Expr {
        self.parse_add_sub()
    }

    fn parse_expr_list(&mut self) -> Vec<Expr> {
        let mut exprs = Vec::new();
        exprs.push(self.parse_expr());
        while matches!(self.peek(), Some(Token::Comma)) {
            self.next();
            exprs.push(self.parse_expr());
        }
        exprs
    }

    fn parse_add_sub(&mut self) -> Expr {
        let mut left = self.parse_mul_div();
        loop {
            let op = match self.peek() {
                Some(Token::Plus) => Op::Add,
                Some(Token::Minus) => Op::Sub,
                _ => break,
            };
            self.next();
            let right = self.parse_mul_div();
            left = Expr::Binary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }
        left
    }

    fn parse_mul_div(&mut self) -> Expr {
        let mut left = self.parse_factor();
        loop {
            let op = match self.peek() {
                Some(Token::Star) => Op::Mul,
                Some(Token::Slash) => Op::Div,
                _ => break,
            };
            self.next();
            let right = self.parse_factor();
            left = Expr::Binary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }
        left
    }

    fn parse_factor(&mut self) -> Expr {
        match self.next() {
            Some(Token::Number(v)) => Expr::Int(v),
            Some(Token::Str(s)) => Expr::Str(s),
            Some(Token::Ident(s)) => Expr::Var(s),
            Some(Token::LParen) => {
                let expr = self.parse_expr();
                self.expect(Token::RParen, "Expected ')'");
                expr
            }
            _ => self.error("Expected expression"),
        }
    }

    fn expect(&mut self, token: Token, msg: &str) {
        match self.next() {
            Some(t) if t == token => {}
            _ => self.error(msg),
        }
    }

    fn expect_end(&mut self) {
        if self.pos != self.tokens.len() {
            self.error("Unexpected tokens at end of line");
        }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    fn next(&mut self) -> Option<Token> {
        if self.pos >= self.tokens.len() {
            None
        } else {
            let tok = self.tokens[self.pos].clone();
            self.pos += 1;
            Some(tok)
        }
    }

    fn error(&self, msg: &str) -> ! {
        die(self.line_no, msg);
    }
}
