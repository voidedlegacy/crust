use crate::ast::{Expr, Op, Program, Stmt};
use crate::lexer::{lex_line, Token};
use crate::util::die;

pub fn parse_program(source: &str) -> Program {
    let lines = lex_lines(source);
    let mut parser = BlockParser::new(lines);
    let stmts = parser.parse_block(None);
    Program { stmts }
}

struct Line {
    line_no: usize,
    tokens: Vec<Token>,
}

fn lex_lines(source: &str) -> Vec<Line> {
    let mut lines = Vec::new();
    for (line_no, line) in source.lines().enumerate() {
        let trimmed = line.trim();
        if trimmed.is_empty() || trimmed.starts_with('#') {
            continue;
        }
        let tokens = lex_line(trimmed, line_no);
        if tokens.is_empty() {
            continue;
        }
        lines.push(Line { line_no, tokens });
    }
    lines
}

struct BlockParser {
    lines: Vec<Line>,
    pos: usize,
}

impl BlockParser {
    fn new(lines: Vec<Line>) -> Self {
        Self { lines, pos: 0 }
    }

    fn parse_block(&mut self, stop_keywords: Option<&[&str]>) -> Vec<Stmt> {
        let mut stmts = Vec::new();
        while self.pos < self.lines.len() {
            if let Some(stop) = stop_keywords {
                if let Some(kw) = self.peek_keyword() {
                    if stop.iter().any(|s| *s == kw) {
                        break;
                    }
                }
            }
            stmts.push(self.parse_stmt());
        }
        stmts
    }

    fn parse_stmt(&mut self) -> Stmt {
        let line = &self.lines[self.pos];
        let line_no = line.line_no;
        let tokens = &line.tokens;
        let (kw, rest) = split_first(tokens, line_no);

        match kw.as_str() {
            "print" => {
                self.pos += 1;
                let mut parser = ExprParser::new(rest.to_vec(), line_no);
                let exprs = parser.parse_expr_list();
                parser.expect_end();
                if exprs.is_empty() {
                    die(line_no, "print expects at least one expression");
                }
                Stmt::Print(exprs)
            }
            "let" => {
                self.pos += 1;
                let mut parser = LineParser::new(rest.to_vec(), line_no);
                let name = match parser.next() {
                    Some(Token::Ident(s)) => s,
                    _ => die(line_no, "Expected identifier after 'let'"),
                };
                parser.expect(Token::Eq, "Expected '=' after variable name");
                let mut expr_parser = ExprParser::new(parser.remaining_tokens(), line_no);
                let expr = expr_parser.parse_expr();
                expr_parser.expect_end();
                Stmt::Let { name, expr }
            }
            "if" => {
                let cond = parse_condition_line(line_no, &rest);
                self.pos += 1;
                let then_body = self.parse_block(Some(&["else", "end"]));
                let else_body = if self.match_keyword_line("else") {
                    self.parse_block(Some(&["end"]))
                } else {
                    Vec::new()
                };
                if !self.match_keyword_line("end") {
                    die(line_no, "Expected 'end' to close if");
                }
                Stmt::If {
                    cond,
                    then_body,
                    else_body,
                }
            }
            "while" => {
                let cond = parse_condition_line(line_no, &rest);
                self.pos += 1;
                let body = self.parse_block(Some(&["end"]));
                if !self.match_keyword_line("end") {
                    die(line_no, "Expected 'end' to close while");
                }
                Stmt::While { cond, body }
            }
            "else" | "end" => {
                die(line_no, "Unexpected block delimiter");
            }
            _ => die(line_no, "Unknown statement. Use 'let', 'print', 'if', or 'while'."),
        }
    }

    fn peek_keyword(&self) -> Option<&str> {
        self.lines.get(self.pos).and_then(|line| match line.tokens.first() {
            Some(Token::Ident(s)) => Some(s.as_str()),
            _ => None,
        })
    }

    fn match_keyword_line(&mut self, keyword: &str) -> bool {
        if self.pos >= self.lines.len() {
            return false;
        }
        let line = &self.lines[self.pos];
        if let Some(Token::Ident(kw)) = line.tokens.first() {
            if kw == keyword && is_line_only_keyword(&line.tokens) {
                self.pos += 1;
                return true;
            }
        }
        false
    }
}

fn parse_condition_line(line_no: usize, tokens: &[Token]) -> Expr {
    let tokens = strip_optional_colon(tokens, line_no);
    if tokens.is_empty() {
        die(line_no, "Expected a condition expression");
    }
    let mut parser = ExprParser::new(tokens.to_vec(), line_no);
    let expr = parser.parse_expr();
    parser.expect_end();
    expr
}

fn is_line_only_keyword(tokens: &[Token]) -> bool {
    match tokens {
        [Token::Ident(_)] => true,
        [Token::Ident(_), Token::Colon] => true,
        _ => false,
    }
}

fn strip_optional_colon<'a>(tokens: &'a [Token], line_no: usize) -> &'a [Token] {
    if matches!(tokens.last(), Some(Token::Colon)) {
        if tokens.len() == 1 {
            die(line_no, "Unexpected ':'");
        }
        &tokens[..tokens.len() - 1]
    } else {
        tokens
    }
}

fn split_first(tokens: &[Token], line_no: usize) -> (String, Vec<Token>) {
    match tokens.first() {
        Some(Token::Ident(s)) => (s.clone(), tokens[1..].to_vec()),
        _ => die(line_no, "Expected a statement"),
    }
}

struct LineParser {
    tokens: Vec<Token>,
    pos: usize,
    line_no: usize,
}

impl LineParser {
    fn new(tokens: Vec<Token>, line_no: usize) -> Self {
        Self {
            tokens,
            pos: 0,
            line_no,
        }
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

    fn expect(&mut self, token: Token, msg: &str) {
        match self.next() {
            Some(t) if t == token => {}
            _ => die(self.line_no, msg),
        }
    }

    fn remaining_tokens(&self) -> Vec<Token> {
        self.tokens[self.pos..].to_vec()
    }
}

struct ExprParser {
    tokens: Vec<Token>,
    pos: usize,
    line_no: usize,
}

impl ExprParser {
    fn new(tokens: Vec<Token>, line_no: usize) -> Self {
        Self {
            tokens,
            pos: 0,
            line_no,
        }
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

    fn parse_expr(&mut self) -> Expr {
        self.parse_compare()
    }

    fn parse_compare(&mut self) -> Expr {
        let mut left = self.parse_add_sub();
        loop {
            let op = match self.peek() {
                Some(Token::EqEq) => Op::Eq,
                Some(Token::BangEq) => Op::Ne,
                Some(Token::Lt) => Op::Lt,
                Some(Token::Lte) => Op::Lte,
                Some(Token::Gt) => Op::Gt,
                Some(Token::Gte) => Op::Gte,
                _ => break,
            };
            self.next();
            let right = self.parse_add_sub();
            left = Expr::Binary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }
        left
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
            Some(Token::Ident(s)) if s == "true" => Expr::Bool(true),
            Some(Token::Ident(s)) if s == "false" => Expr::Bool(false),
            Some(Token::Ident(s)) => Expr::Var(s),
            Some(Token::LParen) => {
                let expr = self.parse_expr();
                self.expect(Token::RParen, "Expected ')'");
                expr
            }
            _ => die(self.line_no, "Expected expression"),
        }
    }

    fn expect(&mut self, token: Token, msg: &str) {
        match self.next() {
            Some(t) if t == token => {}
            _ => die(self.line_no, msg),
        }
    }

    fn expect_end(&mut self) {
        if self.pos != self.tokens.len() {
            die(self.line_no, "Unexpected tokens at end of line");
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
}
