// MIT License
// 
// Copyright (c) 2024 The Fig Programming Language
// 
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

use super::token::Token;

#[derive(Clone, Debug)]
pub struct Lexer {
    position: usize,
    read_position: usize,
    ch: u8,
    pub line_no: u32,
    input: Vec<u8>,
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let mut lex = Lexer {
            position: 0,
            read_position: 0,
            ch: 0,
            line_no: 0,
            input: input.into_bytes(),
        };

        lex.read_char();

        lex
    }

    pub fn next_token(&mut self) -> Result<Token, String> {
        if self.ch == b'\n' {
            self.line_no += 1;
        }

        self.skip_whitespace();

        let tok = match self.ch {
            b'{' => Token::LSquirly,
            b'}' => Token::RSquirly,
            b'(' => Token::Lparen,
            b')' => Token::Rparen,
            b'[' => Token::LBrack,
            b']' => Token::RBrack,
            b',' => Token::Comma,
            b'.' => Token::Period,
            b';' => Token::Semicolon,
            b':' => Token::Colon,
            b'+' => Token::Plus,
            b'-' => Token::Minus,
            b'!' => {
                if self.peek() == b'=' {
                    self.read_char();
                    Token::NotEqual
                } else {
                    Token::Bang
                }
            }
            b'%' => Token::Mod,
            b'>' => Token::GreaterThan,
            b'<' => Token::LessThan,
            b'*' => Token::Asterisk,
            b'/' => Token::ForwardSlash,
            b'=' => {
                if self.peek() == b'=' {
                    self.read_char();
                    Token::Equal
                } else {
                    Token::Assign
                }
            }

            b'"' => Token::String(self.read_string()),
            b'\'' => Token::Char(self.read_char_str()),

            b'&' => Token::Ref,

            b'a'..=b'z' | b'A'..=b'Z' | b'_' => match self.peek() {
                b'0'..=b'9' => {
                    let char = self.ch;
                    self.read_char();
                    let int = self.read_int();
                    self.read_position -= 1;

                    Token::Ident(format!("{}{}", char::from(char), int))
                }

                _ => {
                    let ident = self.read_ident();
                    return Ok(match ident.as_str() {
                        "fn" => Token::Function,
                        "struct" => Token::Struct,
                        "let" => Token::Let,
                        "const" => Token::Const,
                        "import" => Token::Import,
                        "if" => Token::If,
                        "false" => Token::False,
                        "true" => Token::True,
                        "return" => Token::Return,
                        "else" => Token::Else,
                        "export" => Token::Export,
                        "loop" => Token::Loop,
                        "break" => Token::Break,
                        "external" => Token::External,
                        "builtin" => Token::Builtin,
                        _ => Token::Ident(ident),
                    });
                }
            },
            b'0'..=b'9' => return Ok(Token::Int(self.read_int())),
            0 => Token::Eof,
            _ => Token::Illegal,
        };

        self.read_char();

        Ok(tok)
    }

    fn peek(&self) -> u8 {
        if self.read_position >= self.input.len() {
            0
        } else {
            self.input[self.read_position]
        }
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = 0;
        } else {
            self.ch = self.input[self.read_position];
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() || self.ch == b'/' && self.peek() == b'/' {
            if self.ch == b'/' && self.peek() == b'/' {
                self.skip_comment();
            } else {
                self.read_char();
            }
        }
    }

    fn skip_comment(&mut self) {
        while self.ch != b'\n' && self.ch != 0 {
            self.read_char();
        }

        if self.ch == b'\n' {
            self.line_no += 1;
            self.read_char(); // Move past the newline character
        }
    }

    fn read_char_str(&mut self) -> char {
        // Skip the ' char
        self.read_char();

        let ch = self.ch;

        self.read_char();

        char::from(ch)
    }

    fn read_ident(&mut self) -> String {
        let pos = self.position;
        while self.ch.is_ascii_alphabetic() || self.ch == b'_' {
            self.read_char();
        }

        String::from_utf8_lossy(&self.input[pos..self.position]).to_string()
    }

    fn read_int(&mut self) -> String {
        let pos = self.position;
        while self.ch.is_ascii_digit() {
            self.read_char();
        }

        String::from_utf8_lossy(&self.input[pos..self.position]).to_string()
    }

    fn read_string(&mut self) -> String {
        let pos = self.position + 1;

        loop {
            self.read_char();
            if self.ch == b'"' || self.ch == 0 {
                break;
            }
        }

        String::from_utf8_lossy(&self.input[pos..self.position]).to_string()
    }
}

#[cfg(test)]
mod test {
    use super::{Lexer, Token};

    #[test]
    fn get_next_token() {
        let input = "=+(){},;";
        let mut lexer = Lexer::new(input.into());

        let tokens = vec![
            Token::Assign,
            Token::Plus,
            Token::Lparen,
            Token::Rparen,
            Token::LSquirly,
            Token::RSquirly,
            Token::Comma,
            Token::Semicolon,
        ];

        for token in tokens {
            let next_token = lexer.next_token().unwrap();
            assert_eq!(token, next_token);
        }
    }

    #[test]
    fn get_next_complete() {
        let input = r#"let five: i32 = 5;
            let ten = 10;
            let add = fn(x, y) {
                x + y;
            };
            let result = add(five, ten);
        !-/*5;
        5 < 10 > 5;
        if (5 < 10) {
            return true;
        } else {
            return false;
        }

        10 == 10;
        10 != 9;

        "hello world"
        "#;

        let mut lex = Lexer::new(input.into());

        let tokens = vec![
            Token::Let,
            Token::Ident(String::from("five")),
            Token::Colon,
            Token::Ident(String::from("i32")),
            Token::Assign,
            Token::Int(String::from("5")),
            Token::Semicolon,
            Token::Let,
            Token::Ident(String::from("ten")),
            Token::Assign,
            Token::Int(String::from("10")),
            Token::Semicolon,
            Token::Let,
            Token::Ident(String::from("add")),
            Token::Assign,
            Token::Function,
            Token::Lparen,
            Token::Ident(String::from("x")),
            Token::Comma,
            Token::Ident(String::from("y")),
            Token::Rparen,
            Token::LSquirly,
            Token::Ident(String::from("x")),
            Token::Plus,
            Token::Ident(String::from("y")),
            Token::Semicolon,
            Token::RSquirly,
            Token::Semicolon,
            Token::Let,
            Token::Ident(String::from("result")),
            Token::Assign,
            Token::Ident(String::from("add")),
            Token::Lparen,
            Token::Ident(String::from("five")),
            Token::Comma,
            Token::Ident(String::from("ten")),
            Token::Rparen,
            Token::Semicolon,
            Token::Bang,
            Token::Minus,
            Token::ForwardSlash,
            Token::Asterisk,
            Token::Int(String::from("5")),
            Token::Semicolon,
            Token::Int(String::from("5")),
            Token::LessThan,
            Token::Int(String::from("10")),
            Token::GreaterThan,
            Token::Int(String::from("5")),
            Token::Semicolon,
            Token::If,
            Token::Lparen,
            Token::Int(String::from("5")),
            Token::LessThan,
            Token::Int(String::from("10")),
            Token::Rparen,
            Token::LSquirly,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::RSquirly,
            Token::Else,
            Token::LSquirly,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::RSquirly,
            Token::Int(String::from("10")),
            Token::Equal,
            Token::Int(String::from("10")),
            Token::Semicolon,
            Token::Int(String::from("10")),
            Token::NotEqual,
            Token::Int(String::from("9")),
            Token::Semicolon,
            Token::String("hello world".to_string()),
            Token::Eof,
        ];

        for token in tokens {
            let next_token = lex.next_token().unwrap();
            assert_eq!(token, next_token);
        }

        println!("LN: {}", lex.line_no);
    }
}
