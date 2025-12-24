use crate::util::die;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Ident(String),
    Number(i64),
    Str(String),
    Plus,
    Minus,
    Star,
    Slash,
    Eq,
    LParen,
    RParen,
}

pub fn lex_line(line: &str, line_no: usize) -> Vec<Token> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = line.chars().collect();
    let mut i = 0;
    while i < chars.len() {
        let c = chars[i];
        match c {
            ' ' | '\t' => {
                i += 1;
            }
            '#' => break,
            '+' => {
                tokens.push(Token::Plus);
                i += 1;
            }
            '-' => {
                tokens.push(Token::Minus);
                i += 1;
            }
            '*' => {
                tokens.push(Token::Star);
                i += 1;
            }
            '/' => {
                tokens.push(Token::Slash);
                i += 1;
            }
            '=' => {
                tokens.push(Token::Eq);
                i += 1;
            }
            '(' => {
                tokens.push(Token::LParen);
                i += 1;
            }
            ')' => {
                tokens.push(Token::RParen);
                i += 1;
            }
            '"' => {
                let (s, next) = lex_string(&chars, i, line_no);
                tokens.push(Token::Str(s));
                i = next;
            }
            _ if c.is_ascii_digit() => {
                let start = i;
                i += 1;
                while i < chars.len() && chars[i].is_ascii_digit() {
                    i += 1;
                }
                let num: i64 = line[start..i].parse().unwrap_or_else(|_| {
                    die(line_no, "Invalid number literal");
                });
                tokens.push(Token::Number(num));
            }
            _ if is_ident_start(c) => {
                let start = i;
                i += 1;
                while i < chars.len() && is_ident_continue(chars[i]) {
                    i += 1;
                }
                tokens.push(Token::Ident(line[start..i].to_string()));
            }
            _ => die(line_no, "Unexpected character"),
        }
    }

    tokens
}

fn lex_string(chars: &[char], start: usize, line_no: usize) -> (String, usize) {
    let mut out = String::new();
    let mut i = start + 1;
    while i < chars.len() {
        let c = chars[i];
        if c == '"' {
            return (out, i + 1);
        }
        if c == '\\' {
            i += 1;
            if i >= chars.len() {
                die(line_no, "Dangling escape in string");
            }
            let esc = chars[i];
            match esc {
                'n' => out.push('\n'),
                't' => out.push('\t'),
                'r' => out.push('\r'),
                '\\' => out.push('\\'),
                '"' => out.push('"'),
                _ => die(line_no, "Unknown escape in string"),
            }
            i += 1;
            continue;
        }
        out.push(c);
        i += 1;
    }
    die(line_no, "Unterminated string literal");
}

fn is_ident_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_ident_continue(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}
