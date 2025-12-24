use crate::ast::{
    Expr, Field, Function, Op, Param, Program, StructDef, Stmt, Type,
};
use crate::lexer::{lex_line, Token};
use crate::util::die;

pub fn parse_program(source: &str) -> Program {
    let lines = lex_lines(source);
    let mut parser = BlockParser::new(lines);
    parser.parse_program()
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
        let mut tokens = lex_line(trimmed, line_no);
        if tokens.is_empty() {
            continue;
        }
        if let Some((left, right)) = split_rbrace_else(&tokens) {
            lines.push(Line {
                line_no,
                tokens: left,
            });
            tokens = right;
        }
        lines.push(Line { line_no, tokens });
    }
    lines
}

struct BlockParser {
    lines: Vec<Line>,
    pos: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BlockStyle {
    End,
    Brace,
}

impl BlockStyle {
    fn stop_keywords_for_then(self) -> &'static [&'static str] {
        match self {
            BlockStyle::End => &["else", "end"],
            BlockStyle::Brace => &["}"],
        }
    }

    fn stop_keywords_for_loop(self) -> &'static [&'static str] {
        match self {
            BlockStyle::End => &["end"],
            BlockStyle::Brace => &["}"],
        }
    }
}

impl BlockParser {
    fn new(lines: Vec<Line>) -> Self {
        Self { lines, pos: 0 }
    }

    fn parse_program(&mut self) -> Program {
        let mut structs = Vec::new();
        let mut functions = Vec::new();
        let mut stmts = Vec::new();
        while self.pos < self.lines.len() {
            let line = &self.lines[self.pos];
            let line_no = line.line_no;
            let (kw, rest) = split_first(&line.tokens, line_no);
            match kw.as_str() {
                "struct" => {
                    structs.push(self.parse_struct(rest, line_no));
                }
                "fn" => {
                    functions.push(self.parse_function(rest, line_no));
                }
                _ => {
                    stmts.push(self.parse_stmt());
                }
            }
        }
        Program {
            structs,
            functions,
            stmts,
        }
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
                let mut rest = rest;
                strip_trailing_semicolons(&mut rest);
                let mut parser = ExprParser::new(rest, line_no);
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
                let mut expr_tokens = parser.remaining_tokens();
                strip_trailing_semicolons(&mut expr_tokens);
                let mut expr_parser = ExprParser::new(expr_tokens, line_no);
                let expr = expr_parser.parse_expr();
                expr_parser.expect_end();
                Stmt::Let { name, expr }
            }
            "set" => {
                self.pos += 1;
                let mut parser = LineParser::new(rest.to_vec(), line_no);
                let name = match parser.next() {
                    Some(Token::Ident(s)) => s,
                    _ => die(line_no, "Expected identifier after 'set'"),
                };
                parser.expect(Token::Eq, "Expected '=' after variable name");
                let mut expr_tokens = parser.remaining_tokens();
                strip_trailing_semicolons(&mut expr_tokens);
                let mut expr_parser = ExprParser::new(expr_tokens, line_no);
                let expr = expr_parser.parse_expr();
                expr_parser.expect_end();
                Stmt::Set { name, expr }
            }
            "use" => {
                self.pos += 1;
                let mut parser = LineParser::new(rest.to_vec(), line_no);
                let path = match parser.next() {
                    Some(Token::Str(s)) => s,
                    Some(Token::Ident(s)) => s,
                    _ => die(line_no, "Expected string literal or module path after 'use'"),
                };
                let mut alias = None;
                match parser.next() {
                    Some(Token::Ident(s)) if s == "as" => {
                        alias = Some(match parser.next() {
                            Some(Token::Ident(name)) => name,
                            _ => die(line_no, "Expected identifier after 'as'"),
                        });
                        ensure_only_semicolons(parser.remaining_tokens(), line_no);
                    }
                    Some(Token::Semicolon) => {
                        let mut rest = vec![Token::Semicolon];
                        rest.extend(parser.remaining_tokens());
                        ensure_only_semicolons(rest, line_no);
                    }
                    Some(other) => {
                        let mut rest = vec![other];
                        rest.extend(parser.remaining_tokens());
                        ensure_only_semicolons(rest, line_no);
                    }
                    None => {}
                }
                Stmt::Use { path, alias }
            }
            "mod" => {
                self.pos += 1;
                let mut parser = LineParser::new(rest.to_vec(), line_no);
                let name = match parser.next() {
                    Some(Token::Ident(s)) => s,
                    _ => die(line_no, "Expected module name after 'mod'"),
                };
                ensure_only_semicolons(parser.remaining_tokens(), line_no);
                Stmt::Use {
                    path: format!("{}.crust", name),
                    alias: Some(name),
                }
            }
            "return" => {
                self.pos += 1;
                let mut rest = rest;
                strip_trailing_semicolons(&mut rest);
                let mut parser = ExprParser::new(rest, line_no);
                let expr = parser.parse_expr();
                parser.expect_end();
                Stmt::Return(expr)
            }
            "if" => {
                let (cond, style) = parse_condition_line(line_no, &rest);
                self.pos += 1;
                let then_body = self.parse_block(Some(style.stop_keywords_for_then()));
                let else_body = if style == BlockStyle::Brace {
                    if !self.match_keyword_line("}") {
                        die(line_no, "Expected '}' to close if block");
                    }
                    if let Some(else_style) = self.peek_keyword_style("else") {
                        if else_style != BlockStyle::Brace {
                            die(line_no, "Brace-style if requires 'else {'");
                        }
                        self.pos += 1;
                        let else_body = self.parse_block(Some(&["}"]));
                        if !self.match_keyword_line("}") {
                            die(line_no, "Expected '}' to close else block");
                        }
                        else_body
                    } else {
                        Vec::new()
                    }
                } else {
                    let else_body = if let Some(else_style) = self.peek_keyword_style("else") {
                        if else_style != BlockStyle::End {
                            die(line_no, "End-style if requires 'else' without braces");
                        }
                        self.pos += 1;
                        self.parse_block(Some(&["end"]))
                    } else {
                        Vec::new()
                    };
                    if !self.match_keyword_line("end") {
                        die(line_no, "Expected 'end' to close if");
                    }
                    else_body
                };
                Stmt::If {
                    cond,
                    then_body,
                    else_body,
                }
            }
            "while" => {
                let (cond, style) = parse_condition_line(line_no, &rest);
                self.pos += 1;
                let body = self.parse_block(Some(style.stop_keywords_for_loop()));
                if style == BlockStyle::Brace {
                    if !self.match_keyword_line("}") {
                        die(line_no, "Expected '}' to close while block");
                    }
                } else if !self.match_keyword_line("end") {
                    die(line_no, "Expected 'end' to close while");
                }
                Stmt::While { cond, body }
            }
            "else" | "end" => {
                die(line_no, "Unexpected block delimiter");
            }
            _ => die(
                line_no,
                "Unknown statement. Use 'let', 'set', 'use', 'mod', 'print', 'return', 'if', or 'while'.",
            ),
        }
    }

    fn parse_struct(&mut self, rest: Vec<Token>, line_no: usize) -> StructDef {
        let mut parser = LineParser::new(rest, line_no);
        let name = match parser.next() {
            Some(Token::Ident(s)) => s,
            _ => die(line_no, "Expected struct name"),
        };
        let mut rest_tokens = parser.remaining_tokens();
        strip_trailing_semicolons(&mut rest_tokens);
        let mut style = BlockStyle::End;
        if matches!(rest_tokens.last(), Some(Token::LBrace)) {
            rest_tokens.pop();
            style = BlockStyle::Brace;
        }
        if matches!(rest_tokens.last(), Some(Token::Colon)) {
            rest_tokens.pop();
        }
        if !rest_tokens.is_empty() {
            die(line_no, "Unexpected tokens after struct name");
        }
        self.pos += 1;
        let fields = if style == BlockStyle::Brace {
            let fields = self.parse_struct_fields();
            if !self.match_keyword_line("}") {
                die(line_no, "Expected '}' to close struct");
            }
            fields
        } else {
            let fields = self.parse_struct_fields_until_end();
            if !self.match_keyword_line("end") {
                die(line_no, "Expected 'end' to close struct");
            }
            fields
        };
        StructDef { name, fields }
    }

    fn parse_struct_fields(&mut self) -> Vec<Field> {
        let mut fields = Vec::new();
        while self.pos < self.lines.len() {
            if let Some(kw) = self.peek_keyword() {
                if kw == "}" {
                    break;
                }
            }
            let line = &self.lines[self.pos];
            let line_no = line.line_no;
            let tokens = line.tokens.clone();
            fields.push(parse_field_line(tokens, line_no));
            self.pos += 1;
        }
        fields
    }

    fn parse_struct_fields_until_end(&mut self) -> Vec<Field> {
        let mut fields = Vec::new();
        while self.pos < self.lines.len() {
            if let Some(kw) = self.peek_keyword() {
                if kw == "end" {
                    break;
                }
            }
            let line = &self.lines[self.pos];
            let line_no = line.line_no;
            let tokens = line.tokens.clone();
            fields.push(parse_field_line(tokens, line_no));
            self.pos += 1;
        }
        fields
    }

    fn parse_function(&mut self, rest: Vec<Token>, line_no: usize) -> Function {
        let mut parser = LineParser::new(rest, line_no);
        let name = match parser.next() {
            Some(Token::Ident(s)) => s,
            _ => die(line_no, "Expected function name"),
        };
        parser.expect(Token::LParen, "Expected '(' after function name");
        let params = parse_param_list(&mut parser, line_no);
        parser.expect(Token::Arrow, "Expected '->' return type");
        let ret = parse_type_tokens(&mut parser, line_no);
        let mut rest_tokens = parser.remaining_tokens();
        strip_trailing_semicolons(&mut rest_tokens);
        let mut style = BlockStyle::End;
        if matches!(rest_tokens.last(), Some(Token::LBrace)) {
            rest_tokens.pop();
            style = BlockStyle::Brace;
        }
        if matches!(rest_tokens.last(), Some(Token::Colon)) {
            rest_tokens.pop();
        }
        if !rest_tokens.is_empty() {
            die(line_no, "Unexpected tokens after function signature");
        }
        self.pos += 1;
        let body = if style == BlockStyle::Brace {
            let body = self.parse_block(Some(&["}"]));
            if !self.match_keyword_line("}") {
                die(line_no, "Expected '}' to close function");
            }
            body
        } else {
            let body = self.parse_block(Some(&["end"]));
            if !self.match_keyword_line("end") {
                die(line_no, "Expected 'end' to close function");
            }
            body
        };
        Function { name, params, ret, body }
    }

    fn peek_keyword(&self) -> Option<&str> {
        self.lines.get(self.pos).and_then(|line| {
            if is_line_only_rbrace(&line.tokens) {
                Some("}")
            } else {
                match line.tokens.first() {
                    Some(Token::Ident(s)) => Some(s.as_str()),
                    _ => None,
                }
            }
        })
    }

    fn match_keyword_line(&mut self, keyword: &str) -> bool {
        if self.pos >= self.lines.len() {
            return false;
        }
        let line = &self.lines[self.pos];
        if keyword == "}" {
            if is_line_only_rbrace(&line.tokens) {
                self.pos += 1;
                return true;
            }
            return false;
        }
        if let Some(style) = keyword_line_style(&line.tokens, line.line_no, keyword) {
            let _ = style;
            self.pos += 1;
            return true;
        }
        false
    }

    fn peek_keyword_style(&self, keyword: &str) -> Option<BlockStyle> {
        if self.pos >= self.lines.len() {
            return None;
        }
        let line = &self.lines[self.pos];
        keyword_line_style(&line.tokens, line.line_no, keyword)
    }
}

fn parse_condition_line(line_no: usize, tokens: &[Token]) -> (Expr, BlockStyle) {
    let mut tokens = tokens.to_vec();
    strip_trailing_semicolons(&mut tokens);
    let mut style = BlockStyle::End;
    if matches!(tokens.last(), Some(Token::LBrace)) {
        tokens.pop();
        style = BlockStyle::Brace;
    }
    if matches!(tokens.last(), Some(Token::Colon)) {
        tokens.pop();
    }
    if tokens.is_empty() {
        die(line_no, "Expected a condition expression");
    }
    let mut parser = ExprParser::new(tokens, line_no);
    let expr = parser.parse_expr();
    parser.expect_end();
    (expr, style)
}

fn parse_field_line(tokens: Vec<Token>, line_no: usize) -> Field {
    let mut parser = LineParser::new(tokens, line_no);
    let name = match parser.next() {
        Some(Token::Ident(s)) => s,
        _ => die(line_no, "Expected field name"),
    };
    parser.expect(Token::Colon, "Expected ':' in field");
    let ty = parse_type_tokens(&mut parser, line_no);
    ensure_only_semicolons(parser.remaining_tokens(), line_no);
    Field { name, ty }
}

fn parse_param_list(parser: &mut LineParser, line_no: usize) -> Vec<Param> {
    let mut params = Vec::new();
    if matches!(parser.peek(), Some(Token::RParen)) {
        parser.next();
        return params;
    }
    loop {
        let name = match parser.next() {
            Some(Token::Ident(s)) => s,
            _ => die(line_no, "Expected parameter name"),
        };
        parser.expect(Token::Colon, "Expected ':' after parameter name");
        let ty = parse_type_tokens(parser, line_no);
        params.push(Param { name, ty });
        match parser.next() {
            Some(Token::Comma) => continue,
            Some(Token::RParen) => break,
            _ => die(line_no, "Expected ',' or ')' in parameter list"),
        }
    }
    params
}

fn parse_type_tokens(parser: &mut LineParser, line_no: usize) -> Type {
    match parser.next() {
        Some(Token::Ident(s)) if s == "int" => Type::Int,
        Some(Token::Ident(s)) if s == "str" => Type::Str,
        Some(Token::Ident(s)) if s == "bool" => Type::Bool,
        Some(Token::Ident(s)) => Type::Struct(s),
        _ => die(line_no, "Expected type name"),
    }
}

fn keyword_line_style(tokens: &[Token], line_no: usize, keyword: &str) -> Option<BlockStyle> {
    let mut iter = tokens.iter();
    match iter.next() {
        Some(Token::Ident(s)) if s == keyword => {}
        _ => return None,
    }
    let mut rest: Vec<Token> = iter.cloned().collect();
    strip_trailing_semicolons(&mut rest);
    let mut style = BlockStyle::End;
    if matches!(rest.last(), Some(Token::LBrace)) {
        rest.pop();
        style = BlockStyle::Brace;
    }
    if matches!(rest.last(), Some(Token::Colon)) {
        rest.pop();
    }
    if !rest.is_empty() {
        die(line_no, "Unexpected tokens after keyword");
    }
    Some(style)
}

fn is_line_only_rbrace(tokens: &[Token]) -> bool {
    let mut rest = tokens.to_vec();
    strip_trailing_semicolons(&mut rest);
    matches!(rest.as_slice(), [Token::RBrace])
}

fn strip_trailing_semicolons(tokens: &mut Vec<Token>) {
    while matches!(tokens.last(), Some(Token::Semicolon)) {
        tokens.pop();
    }
}

fn ensure_only_semicolons(tokens: Vec<Token>, line_no: usize) {
    let mut rest = tokens;
    strip_trailing_semicolons(&mut rest);
    if !rest.is_empty() {
        die(line_no, "Unexpected tokens after statement");
    }
}

fn split_first(tokens: &[Token], line_no: usize) -> (String, Vec<Token>) {
    match tokens.first() {
        Some(Token::Ident(s)) => (s.clone(), tokens[1..].to_vec()),
        Some(Token::RBrace) => die(line_no, "Unexpected '}'"),
        _ => die(line_no, "Expected a statement"),
    }
}

fn split_rbrace_else(tokens: &[Token]) -> Option<(Vec<Token>, Vec<Token>)> {
    if !matches!(tokens.first(), Some(Token::RBrace)) {
        return None;
    }
    let mut idx = 1;
    while idx < tokens.len() && matches!(tokens[idx], Token::Semicolon) {
        idx += 1;
    }
    if idx < tokens.len() {
        if let Token::Ident(ref s) = tokens[idx] {
            if s == "else" {
                let left = tokens[..1].to_vec();
                let right = tokens[idx..].to_vec();
                return Some((left, right));
            }
        }
    }
    None
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

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
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
        let mut expr = match self.next() {
            Some(Token::Number(v)) => Expr::Int(v),
            Some(Token::Str(s)) => Expr::Str(s),
            Some(Token::Ident(s)) if s == "true" => Expr::Bool(true),
            Some(Token::Ident(s)) if s == "false" => Expr::Bool(false),
            Some(Token::Ident(s)) => {
                if matches!(self.peek(), Some(Token::LParen)) {
                    self.next();
                    let args = self.parse_call_args();
                    Expr::Call { name: s, args }
                } else if matches!(self.peek(), Some(Token::LBrace)) {
                    self.next();
                    let fields = self.parse_struct_init_fields();
                    Expr::StructInit { name: s, fields }
                } else {
                    Expr::Var(s)
                }
            }
            Some(Token::LParen) => {
                let expr = self.parse_expr();
                self.expect(Token::RParen, "Expected ')'");
                expr
            }
            _ => die(self.line_no, "Expected expression"),
        };

        while matches!(self.peek(), Some(Token::Dot)) {
            self.next();
            let name = match self.next() {
                Some(Token::Ident(s)) => s,
                _ => die(self.line_no, "Expected field name after '.'"),
            };
            expr = Expr::Field {
                base: Box::new(expr),
                name,
            };
        }

        expr
    }

    fn expect(&mut self, token: Token, msg: &str) {
        match self.next() {
            Some(t) if t == token => {}
            _ => die(self.line_no, msg),
        }
    }

    fn parse_call_args(&mut self) -> Vec<Expr> {
        if matches!(self.peek(), Some(Token::RParen)) {
            self.next();
            return Vec::new();
        }
        let mut args = Vec::new();
        args.push(self.parse_expr());
        while matches!(self.peek(), Some(Token::Comma)) {
            self.next();
            args.push(self.parse_expr());
        }
        self.expect(Token::RParen, "Expected ')'");
        args
    }

    fn parse_struct_init_fields(&mut self) -> Vec<(String, Expr)> {
        if matches!(self.peek(), Some(Token::RBrace)) {
            self.next();
            return Vec::new();
        }
        let mut fields = Vec::new();
        loop {
            let name = match self.next() {
                Some(Token::Ident(s)) => s,
                _ => die(self.line_no, "Expected field name"),
            };
            self.expect(Token::Colon, "Expected ':' in struct initializer");
            let expr = self.parse_expr();
            fields.push((name, expr));
            match self.peek() {
                Some(Token::Comma) => {
                    self.next();
                }
                Some(Token::RBrace) => {
                    self.next();
                    break;
                }
                _ => die(self.line_no, "Expected ',' or '}' in struct initializer"),
            }
        }
        fields
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
