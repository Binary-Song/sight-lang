use crate::span::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    EqEq(Span),
    NotEq(Span),
    Le(Span),
    Ge(Span),
    AndAnd(Span),
    OrOr(Span),
    Arrow(Span),
    FatArrow(Span),

    Plus(Span),
    Minus(Span),
    Star(Span),
    Slash(Span),
    Percent(Span),
    Not(Span),
    Lt(Span),
    Gt(Span),
    Eq(Span),

    LParen(Span),
    RParen(Span),
    LBrace(Span),
    RBrace(Span),

    Comma(Span),
    Semicolon(Span),
    Colon(Span),
    Dot(Span),

    IntLit(String, Span),

    True(Span),
    False(Span),
    Fn(Span),
    Let(Span),
    Bool(Span),
    Int(Span),

    Ident(String, Span),
    BadUtf8Char(u8, Span), // 新增：非法UTF-8字节
    Eof(Span),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenType {
    EqEq,
    NotEq,
    Le,
    Ge,
    AndAnd,
    OrOr,
    Arrow,
    FatArrow,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Not,
    Lt,
    Gt,
    Eq,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Semicolon,
    Colon,
    Dot,
    IntLit,
    True,
    False,
    Fn,
    Let,
    Bool,
    Int,
    Ident,
    Eof,
    BadUtf8Char,
}

pub struct Lexer<'a> {
    pub input: &'a str,
    pub pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self { input, pos: 0 }
    }

    fn skip_whitespace(&mut self) {
        while self.peek_char().is_whitespace() {
            self.next_char();
        }
    }

    pub fn peek_char(&self) -> char {
        if self.pos >= self.input.len() {
            '\0'
        } else {
            let bytes = self.input.as_bytes();
            let s = &bytes[self.pos..];
            match std::str::from_utf8(s) {
                Ok(s) => s.chars().next().unwrap_or('\0'),
                Err(e) => {
                    if let Some(_) = e.error_len() {
                        '\u{FFFD}'
                    } else {
                        '\u{FFFD}'
                    }
                }
            }
        }
    }

    /// 返回当前位置字符并前进，越界返回 '\0'，非法UTF-8返回 '\u{FFFD}'
    pub fn next_char(&mut self) -> char {
        if self.pos >= self.input.len() {
            self.pos += 1;
            '\0'
        } else {
            let bytes = self.input.as_bytes();
            let s = &bytes[self.pos..];
            match std::str::from_utf8(s) {
                Ok(s) => {
                    let mut chars = s.chars();
                    let c = chars.next().unwrap_or('\0');
                    self.pos += c.len_utf8();
                    c
                }
                Err(_) => {
                    self.pos += 1;
                    '\u{FFFD}'
                }
            }
        }
    }

    /// 匹配字符串，支持EOF和非法UTF-8
    fn match_str(&mut self, s: &str) -> Option<Span> {
        let mut temp_pos = self.pos;
        let mut matched = true;
        for expected in s.chars() {
            let actual = if temp_pos < self.input.len() {
                let bytes = self.input.as_bytes();
                let slice = &bytes[temp_pos..];
                match std::str::from_utf8(slice) {
                    Ok(s) => s.chars().next().unwrap_or('\0'),
                    Err(_) => '\u{FFFD}',
                }
            } else {
                '\0'
            };
            if actual != expected {
                matched = false;
                break;
            }
            temp_pos += actual.len_utf8();
        }
        if matched {
            let start = self.pos;
            self.pos = temp_pos;
            Some(Span(start, self.pos))
        } else {
            None
        }
    }

    pub fn peek_token(&mut self) -> Token {
        let pos_backup = self.pos;
        let token = self.next_token();
        self.pos = pos_backup;
        token
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let start = self.pos;
        let c = self.peek_char();

        // EOF
        if c == '\0' {
            self.next_char();
            return Token::Eof(Span(start, self.pos));
        }

        // 非法UTF-8
        if c == '\u{FFFD}' {
            let bad_byte = self.input.as_bytes().get(self.pos).copied().unwrap_or(0);
            self.next_char();
            return Token::BadUtf8Char(bad_byte, Span(start, self.pos));
        }

        // 优先匹配双字符操作符
        let two_char_ops = [
            ("==", Token::EqEq as fn(Span) -> Token),
            ("!=", Token::NotEq),
            ("<=", Token::Le),
            (">=", Token::Ge),
            ("&&", Token::AndAnd),
            ("||", Token::OrOr),
            ("->", Token::Arrow),
            ("=>", Token::FatArrow),
        ];
        for (op, constructor) in two_char_ops.iter() {
            if let Some(span) = self.match_str(op) {
                return constructor(span);
            }
        }

        let c = self.peek_char();
        let start = self.pos;

        let token = match c {
            '+' => {
                self.next_char();
                Token::Plus(Span(start, self.pos))
            }
            '-' => {
                self.next_char();
                Token::Minus(Span(start, self.pos))
            }
            '*' => {
                self.next_char();
                Token::Star(Span(start, self.pos))
            }
            '/' => {
                self.next_char();
                Token::Slash(Span(start, self.pos))
            }
            '%' => {
                self.next_char();
                Token::Percent(Span(start, self.pos))
            }
            '!' => {
                self.next_char();
                Token::Not(Span(start, self.pos))
            }
            '=' => {
                self.next_char();
                Token::Eq(Span(start, self.pos))
            }
            '<' => {
                self.next_char();
                Token::Lt(Span(start, self.pos))
            }
            '>' => {
                self.next_char();
                Token::Gt(Span(start, self.pos))
            }
            '(' => {
                self.next_char();
                Token::LParen(Span(start, self.pos))
            }
            ')' => {
                self.next_char();
                Token::RParen(Span(start, self.pos))
            }
            '{' => {
                self.next_char();
                Token::LBrace(Span(start, self.pos))
            }
            '}' => {
                self.next_char();
                Token::RBrace(Span(start, self.pos))
            }
            ',' => {
                self.next_char();
                Token::Comma(Span(start, self.pos))
            }
            ';' => {
                self.next_char();
                Token::Semicolon(Span(start, self.pos))
            }
            ':' => {
                self.next_char();
                Token::Colon(Span(start, self.pos))
            }
            '.' => {
                self.next_char();
                Token::Dot(Span(start, self.pos))
            }
            _ => {
                // 不是单字符操作符
                if c.is_ascii_digit() {
                    let start = self.pos;
                    while self.peek_char().is_ascii_digit() {
                        self.next_char();
                    }
                    let end = self.pos;
                    let s = &self.input[start..end];
                    Token::IntLit(s.to_string(), Span(start, end))
                } else if c.is_ascii_alphabetic() || c == '_' {
                    let start = self.pos;
                    while {
                        let c = self.peek_char();
                        c.is_ascii_alphanumeric() || c == '_'
                    } {
                        self.next_char();
                    }
                    let end = self.pos;
                    let s = &self.input[start..end];
                    match s {
                        "true" => Token::True(Span(start, end)),
                        "false" => Token::False(Span(start, end)),
                        "fn" => Token::Fn(Span(start, end)),
                        "let" => Token::Let(Span(start, end)),
                        "bool" => Token::Bool(Span(start, end)),
                        "int" => Token::Int(Span(start, end)),
                        _ => Token::Ident(s.to_string(), Span(start, end)),
                    }
                } else {
                    // 跳过无法识别的字符
                    self.next_char();
                    self.next_token()
                }
            }
        };
        token
    }
}

impl Token {
    pub fn token_type(&self) -> TokenType {
        match self {
            Token::EqEq(_) => TokenType::EqEq,
            Token::NotEq(_) => TokenType::NotEq,
            Token::Le(_) => TokenType::Le,
            Token::Ge(_) => TokenType::Ge,
            Token::AndAnd(_) => TokenType::AndAnd,
            Token::OrOr(_) => TokenType::OrOr,
            Token::Arrow(_) => TokenType::Arrow,
            Token::FatArrow(_) => TokenType::FatArrow,
            Token::Plus(_) => TokenType::Plus,
            Token::Minus(_) => TokenType::Minus,
            Token::Star(_) => TokenType::Star,
            Token::Slash(_) => TokenType::Slash,
            Token::Percent(_) => TokenType::Percent,
            Token::Not(_) => TokenType::Not,
            Token::Eq(_) => TokenType::Eq,
            Token::Lt(_) => TokenType::Lt,
            Token::Gt(_) => TokenType::Gt,
            Token::LParen(_) => TokenType::LParen,
            Token::RParen(_) => TokenType::RParen,
            Token::LBrace(_) => TokenType::LBrace,
            Token::RBrace(_) => TokenType::RBrace,
            Token::Comma(_) => TokenType::Comma,
            Token::Semicolon(_) => TokenType::Semicolon,
            Token::Colon(_) => TokenType::Colon,
            Token::Dot(_) => TokenType::Dot,
            Token::IntLit(_, _) => TokenType::IntLit,
            Token::True(_) => TokenType::True,
            Token::False(_) => TokenType::False,
            Token::Fn(_) => TokenType::Fn,
            Token::Let(_) => TokenType::Let,
            Token::Bool(_) => TokenType::Bool,
            Token::Int(_) => TokenType::Int,
            Token::Ident(_, _) => TokenType::Ident,
            Token::Eof(_) => TokenType::Eof,
            Token::BadUtf8Char(_, _) => TokenType::BadUtf8Char,
        }
    }
}

impl TokenType {
    pub fn name(&self) -> &'static str {
        match self {
            TokenType::EqEq => "EqEq",
            TokenType::NotEq => "NotEq",
            TokenType::Le => "Le",
            TokenType::Ge => "Ge",
            TokenType::AndAnd => "AndAnd",
            TokenType::OrOr => "OrOr",
            TokenType::Arrow => "Arrow",
            TokenType::FatArrow => "FatArrow",
            TokenType::Plus => "Plus",
            TokenType::Minus => "Minus",
            TokenType::Star => "Star",
            TokenType::Slash => "Slash",
            TokenType::Percent => "Percent",
            TokenType::Not => "Not",
            TokenType::Lt => "Lt",
            TokenType::Gt => "Gt",
            TokenType::Eq => "Eq",
            TokenType::LParen => "LParen",
            TokenType::RParen => "RParen",
            TokenType::LBrace => "LBrace",
            TokenType::RBrace => "RBrace",
            TokenType::Comma => "Comma",
            TokenType::Semicolon => "Semicolon",
            TokenType::Colon => "Colon",
            TokenType::Dot => "Dot",
            TokenType::IntLit => "IntLit",
            TokenType::True => "True",
            TokenType::False => "False",
            TokenType::Fn => "Fn",
            TokenType::Let => "Let",
            TokenType::Bool => "Bool",
            TokenType::Int => "Int",
            TokenType::Ident => "Ident",
            TokenType::Eof => "Eof",
            TokenType::BadUtf8Char => "BadUtf8Char",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    fn lex_all(input: &str) -> Vec<Token> {
        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();
        loop {
            let tok = lexer.next_token();
            if let Token::Eof(_) = tok {
                break;
            }
            tokens.push(tok);
        }
        tokens
    }

    #[test]
    fn test_single_char_tokens() {
        let input = "+-*/%!<>(){};,.:=";
        let tokens = lex_all(input);
        let expected = vec![
            Token::Plus(Span(0, 1)),
            Token::Minus(Span(1, 2)),
            Token::Star(Span(2, 3)),
            Token::Slash(Span(3, 4)),
            Token::Percent(Span(4, 5)),
            Token::Not(Span(5, 6)),
            Token::Lt(Span(6, 7)),
            Token::Gt(Span(7, 8)),
            Token::LParen(Span(8, 9)),
            Token::RParen(Span(9, 10)),
            Token::LBrace(Span(10, 11)),
            Token::RBrace(Span(11, 12)),
            Token::Semicolon(Span(12, 13)),
            Token::Comma(Span(13, 14)),
            Token::Dot(Span(14, 15)),
            Token::Colon(Span(15, 16)),
            Token::Eq(Span(16, 17)),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_two_char_tokens() {
        let input = "== != <= >= && || -> =>";
        let tokens = lex_all(input);
        let expected = vec![
            Token::EqEq(Span(0, 2)),
            Token::NotEq(Span(3, 5)),
            Token::Le(Span(6, 8)),
            Token::Ge(Span(9, 11)),
            Token::AndAnd(Span(12, 14)),
            Token::OrOr(Span(15, 17)),
            Token::Arrow(Span(18, 20)),
            Token::FatArrow(Span(21, 23)),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_integer_literals() {
        let input = "42 123456 0";
        let tokens = lex_all(input);
        let expected = vec![
            Token::IntLit("42".into(), Span(0, 2)),
            Token::IntLit("123456".into(), Span(3, 9)),
            Token::IntLit("0".into(), Span(10, 11)),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_identifiers_and_keywords() {
        let input = "true false fn let bool int _id var123";
        let tokens = lex_all(input);
        let expected = vec![
            Token::True(Span(0, 4)),
            Token::False(Span(5, 10)),
            Token::Fn(Span(11, 13)),
            Token::Let(Span(14, 17)),
            Token::Bool(Span(18, 22)),
            Token::Int(Span(23, 26)),
            Token::Ident("_id".into(), Span(27, 30)),
            Token::Ident("var123".into(), Span(31, 37)),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_whitespace_handling() {
        let input = "  \t\n  let    x = 10  ";
        let tokens = lex_all(input);
        let expected = vec![
            Token::Let(Span(6, 9)),
            Token::Ident("x".into(), Span(13, 14)),
            Token::Eq(Span(15, 16)),
            Token::IntLit("10".into(), Span(17, 19)),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_mixed_expression() {
        let input = "let x = a + 10 == 20;";
        let tokens = lex_all(input);
        let expected = vec![
            Token::Let(Span(0, 3)),
            Token::Ident("x".into(), Span(4, 5)),
            Token::Eq(Span(6, 7)),
            Token::Ident("a".into(), Span(8, 9)),
            Token::Plus(Span(10, 11)),
            Token::IntLit("10".into(), Span(12, 14)),
            Token::EqEq(Span(15, 17)),
            Token::IntLit("20".into(), Span(18, 20)),
            Token::Semicolon(Span(20, 21)),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_unexpected_characters_are_skipped() {
        let input = "#@ let $x = 42;";
        let tokens = lex_all(input);
        let expected = vec![
            Token::Let(Span(3, 6)),
            Token::Ident("x".into(), Span(8, 9)),
            Token::Eq(Span(10, 11)),
            Token::IntLit("42".into(), Span(12, 14)),
            Token::Semicolon(Span(14, 15)),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_spans_are_correct() {
        let input = "fn my_var = 123;";
        let tokens = lex_all(input);
        let spans: Vec<Span> = tokens
            .iter()
            .map(|t| match t {
                Token::Fn(span)
                | Token::Ident(_, span)
                | Token::Eq(span)
                | Token::IntLit(_, span)
                | Token::Semicolon(span) => span.clone(),
                _ => panic!("unexpected token in test"),
            })
            .collect();
        let expected_spans = vec![
            Span(0, 2),
            Span(3, 9),
            Span(10, 11),
            Span(12, 15),
            Span(15, 16),
        ];
        assert_eq!(spans, expected_spans);
    }
}
