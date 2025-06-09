#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    EqEq((usize, usize)),
    NotEq((usize, usize)),
    Le((usize, usize)),
    Ge((usize, usize)),
    AndAnd((usize, usize)),
    OrOr((usize, usize)),
    Arrow((usize, usize)),
    FatArrow((usize, usize)),

    Plus((usize, usize)),
    Minus((usize, usize)),
    Star((usize, usize)),
    Slash((usize, usize)),
    Percent((usize, usize)),
    Not((usize, usize)),
    Lt((usize, usize)),
    Gt((usize, usize)),
    Eq((usize, usize)),

    LParen((usize, usize)),
    RParen((usize, usize)),
    LBrace((usize, usize)),
    RBrace((usize, usize)),

    Comma((usize, usize)),
    Semicolon((usize, usize)),
    Colon((usize, usize)),
    Dot((usize, usize)),

    IntLit(String, (usize, usize)),

    True((usize, usize)),
    False((usize, usize)),
    Fn((usize, usize)),
    Let((usize, usize)),
    Bool((usize, usize)),
    Int((usize, usize)),

    Ident(String, (usize, usize)),
    BadUtf8Char(u8, (usize, usize)), // 新增：非法UTF-8字节
    Eof((usize, usize)),
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

    /// 返回当前位置的字符，越界返回 '\0'，非法UTF-8返回 '\u{FFFD}'
    pub fn peek_char(&self) -> char {
        if self.pos >= self.input.len() {
            '\0'
        } else {
            let bytes = self.input.as_bytes();
            let s = &bytes[self.pos..];
            match std::str::from_utf8(s) {
                Ok(s) => s.chars().next().unwrap_or('\0'),
                Err(e) => {
                    if let Some(invalid_idx) = e.error_len() {
                        // 立即遇到非法字节
                        '\u{FFFD}'
                    } else {
                        // 不完整的UTF-8序列
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
                Err(e) => {
                    // 非法UTF-8字节
                    let bad = bytes[self.pos];
                    self.pos += 1;
                    '\u{FFFD}'
                }
            }
        }
    }

    /// 匹配字符串，支持EOF和非法UTF-8
    fn match_str(&mut self, s: &str) -> Option<(usize, usize)> {
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
            Some((start, self.pos))
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
            return Token::Eof((start, self.pos));
        }

        // 非法UTF-8
        if c == '\u{FFFD}' {
            let bad_byte = self.input.as_bytes().get(self.pos).copied().unwrap_or(0);
            self.next_char();
            return Token::BadUtf8Char(bad_byte, (start, self.pos));
        }

        // 优先匹配双字符操作符
        let two_char_ops = [
            ("==", Token::EqEq as fn((usize, usize)) -> Token),
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
                Token::Plus((start, self.pos))
            }
            '-' => {
                self.next_char();
                Token::Minus((start, self.pos))
            }
            '*' => {
                self.next_char();
                Token::Star((start, self.pos))
            }
            '/' => {
                self.next_char();
                Token::Slash((start, self.pos))
            }
            '%' => {
                self.next_char();
                Token::Percent((start, self.pos))
            }
            '!' => {
                self.next_char();
                Token::Not((start, self.pos))
            }
            '=' => {
                self.next_char();
                Token::Eq((start, self.pos))
            }
            '<' => {
                self.next_char();
                Token::Lt((start, self.pos))
            }
            '>' => {
                self.next_char();
                Token::Gt((start, self.pos))
            }
            '(' => {
                self.next_char();
                Token::LParen((start, self.pos))
            }
            ')' => {
                self.next_char();
                Token::RParen((start, self.pos))
            }
            '{' => {
                self.next_char();
                Token::LBrace((start, self.pos))
            }
            '}' => {
                self.next_char();
                Token::RBrace((start, self.pos))
            }
            ',' => {
                self.next_char();
                Token::Comma((start, self.pos))
            }
            ';' => {
                self.next_char();
                Token::Semicolon((start, self.pos))
            }
            ':' => {
                self.next_char();
                Token::Colon((start, self.pos))
            }
            '.' => {
                self.next_char();
                Token::Dot((start, self.pos))
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
                    Token::IntLit(s.to_string(), (start, end))
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
                        "true" => Token::True((start, end)),
                        "false" => Token::False((start, end)),
                        "fn" => Token::Fn((start, end)),
                        "let" => Token::Let((start, end)),
                        "bool" => Token::Bool((start, end)),
                        "int" => Token::Int((start, end)),
                        _ => Token::Ident(s.to_string(), (start, end)),
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
    use pretty_assertions::assert_ne;

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
            Token::Plus((0, 1)),
            Token::Minus((1, 2)),
            Token::Star((2, 3)),
            Token::Slash((3, 4)),
            Token::Percent((4, 5)),
            Token::Not((5, 6)),
            Token::Lt((6, 7)),
            Token::Gt((7, 8)),
            Token::LParen((8, 9)),
            Token::RParen((9, 10)),
            Token::LBrace((10, 11)),
            Token::RBrace((11, 12)),
            Token::Semicolon((12, 13)),
            Token::Comma((13, 14)),
            Token::Dot((14, 15)),
            Token::Colon((15, 16)),
            Token::Eq((16, 17)),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_two_char_tokens() {
        let input = "== != <= >= && || -> =>";
        let tokens = lex_all(input);
        let expected = vec![
            Token::EqEq((0, 2)),
            Token::NotEq((3, 5)),
            Token::Le((6, 8)),
            Token::Ge((9, 11)),
            Token::AndAnd((12, 14)),
            Token::OrOr((15, 17)),
            Token::Arrow((18, 20)),
            Token::FatArrow((21, 23)),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_integer_literals() {
        let input = "42 123456 0";
        let tokens = lex_all(input);
        let expected = vec![
            Token::IntLit("42".into(), (0, 2)),
            Token::IntLit("123456".into(), (3, 9)),
            Token::IntLit("0".into(), (10, 11)),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_identifiers_and_keywords() {
        let input = "true false fn let bool int _id var123";
        let tokens = lex_all(input);
        let expected = vec![
            Token::True((0, 4)),
            Token::False((5, 10)),
            Token::Fn((11, 13)),
            Token::Let((14, 17)),
            Token::Bool((18, 22)),
            Token::Int((23, 26)),
            Token::Ident("_id".into(), (27, 30)),
            Token::Ident("var123".into(), (31, 37)),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_whitespace_handling() {
        let input = "  \t\n  let    x = 10  ";
        let tokens = lex_all(input);
        let expected = vec![
            Token::Let((6, 9)),
            Token::Ident("x".into(), (13, 14)),
            Token::Eq((15, 16)),
            Token::IntLit("10".into(), (17, 19)),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_mixed_expression() {
        let input = "let x = a + 10 == 20;";
        let tokens = lex_all(input);
        let expected = vec![
            Token::Let((0, 3)),
            Token::Ident("x".into(), (4, 5)),
            Token::Eq((6, 7)),
            Token::Ident("a".into(), (8, 9)),
            Token::Plus((10, 11)),
            Token::IntLit("10".into(), (12, 14)),
            Token::EqEq((15, 17)),
            Token::IntLit("20".into(), (18, 20)),
            Token::Semicolon((20, 21)),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_unexpected_characters_are_skipped() {
        let input = "#@ let $x = 42;";
        let tokens = lex_all(input);
        let expected = vec![
            Token::Let((3, 6)),
            Token::Ident("x".into(), (8, 9)),
            Token::Eq((10, 11)),
            Token::IntLit("42".into(), (12, 14)),
            Token::Semicolon((14, 15)),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_spans_are_correct() {
        let input = "fn my_var = 123;";
        let tokens = lex_all(input);
        let spans: Vec<(usize, usize)> = tokens
            .iter()
            .map(|t| match t {
                Token::Fn(span)
                | Token::Ident(_, span)
                | Token::Eq(span)
                | Token::IntLit(_, span)
                | Token::Semicolon(span) => *span,
                _ => panic!("unexpected token in test"),
            })
            .collect();
        let expected_spans = vec![(0, 2), (3, 9), (10, 11), (12, 15), (15, 16)];
        assert_eq!(spans, expected_spans);
    }
}
