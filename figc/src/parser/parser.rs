use std::fmt::{Display, Formatter};
use std::mem;
use std::u32::MAX;

use crate::lexer::{lexer::Lexer, token::Token};

#[derive(Debug, Clone, PartialEq, PartialOrd)]
#[repr(u8)]
pub enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

impl Precedence {
    pub fn from_token(token: &Token) -> Option<Self> {
        match token {
            Token::Equal => Some(Self::Equals),
            Token::NotEqual => Some(Self::Equals),
            Token::LessThan => Some(Self::LessGreater),
            Token::GreaterThan => Some(Self::LessGreater),
            Token::Plus => Some(Self::Sum),
            Token::Minus => Some(Self::Sum),
            Token::ForwardSlash => Some(Self::Product),
            Token::Mod => Some(Self::Product),
            Token::Asterisk => Some(Self::Product),
            Token::Lparen => Some(Self::Call),

            _ => None,
        }
    }
}

pub trait Parse<'a> {
    fn parse(parser: &mut Parser<'a>, precedence: Option<Precedence>) -> PResult<Self>
    where
        Self: Sized;
}

#[derive(Debug)]
pub struct Parser<'a> {
    lexer: &'a mut Lexer,
    pub current_token: Token,
    pub next_token: Token,
    // TODO: errors: Vec<ParserError>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParserError {
    Unexpected(String),
    Expected(String),
}

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::Unexpected(msg) => write!(f, "{}", msg),
            ParserError::Expected(msg) => write!(f, "{}", msg)
        }
    }
}

impl ParserError {
    pub fn unexpected(what: String, line: u32) -> Self {
        Self::Unexpected(format!("Unexpected token: {} at line {}", what, line))
    }

    pub fn expected<T>(expected: T, found: T, line: u32) -> Self
    where
        T: Display,
    {
        Self::Expected(format!("expected {} found {} at line {}", expected, found, line))
    }
}

pub(super) type PResult<T> = Result<T, ParserError>;

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer) -> Self {
        let current_token = lexer.next_token().unwrap();
        let next_token = lexer.next_token().unwrap();

        Self {
            lexer,
            current_token,
            next_token,
        }
    }

    pub(super) fn current_token_is(&self, token: Token) -> bool {
        self.current_token == token
    }

    pub(super) fn next_token(&mut self) {
        self.current_token = self.lexer.next_token().unwrap();
        mem::swap(&mut self.current_token, &mut self.next_token);
    }

    pub(super) fn next_token_is(&self, token: Token) -> bool {
        self.next_token == token
    }

    pub(super) fn next_precedence(&self) -> Option<Precedence> {
        Precedence::from_token(&self.next_token)
    }

    pub(super) fn current_precedence(&self) -> Option<Precedence> {
        Precedence::from_token(&self.current_token)
    }

    pub(super) fn expect_peek(&mut self, expected: Token) -> PResult<()> {
        if !self.next_token_is(expected.clone()) {
            return Err(ParserError::expected(
                expected.to_string(),
                self.next_token.to_string(),
                self.lexer.line_no,
            ));
        }

        self.next_token();

        Ok(())
    }

    pub fn current_line(&self) -> u32 {
        self.lexer.line_no
    }
}
