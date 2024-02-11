use std::fmt::Display;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Token {
    // Special tokens
    Illegal,
    Eof,

    // Identifiers + literals
    Ident(String),
    Int(i64),

    // Operators
    Assign,
    Plus,
    Minus,
    Bang, // !
    Slash,
    Asterisk,
    Eq,    // ==
    NotEq, // !=
    LessThan,
    GreaterThan,

    // Delimiters
    Comma,
    SemiColon,
    LParen,
    RParen,
    LBrace,
    RBrace,

    // Keywords
    Function,
    Let,
    If,
    Else,
    Return,
    True,
    False,
}

impl Token {
    #[must_use]
    pub fn lookup_ident(ident: &str) -> Token {
        match ident {
            "fn" => Token::Function,
            "let" => Token::Let,
            "if" => Token::If,
            "else" => Token::Else,
            "return" => Token::Return,
            "true" => Token::True,
            "false" => Token::False,
            _ => Token::Ident(ident.to_string()),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Bang => write!(f, "!"),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Slash => write!(f, "/"),
            Token::Asterisk => write!(f, "*"),
            Token::Eq => write!(f, "=="),
            Token::NotEq => write!(f, "!="),
            Token::LessThan => write!(f, "<"),
            Token::GreaterThan => write!(f, ">"),
            _ => write!(f, "{self:?}"),
        }
    }
}
