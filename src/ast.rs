#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub span: (usize, usize),
    pub kind: TokenKind,
}

// see syntax.rs for the grammar
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    EQEQ,
    NE,
    LT,
    LE,
    GT,
    GE,
    AMPAMP,
    PIPEPIPE,
    ARROW,
    PLUS,
    MINUS,
    STAR,
    SLASH,
    PERCENT,
    EXCLAM,
    EQ,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    COMMA,
    SEMICOLON,
    DOT,
    INTLIT,
    IDENT,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Unary(Token, Box<Expr>),
    Binary(Token, Box<Expr>, Box<Expr>),
    IntLit(Token),
    Var(Token),
}
