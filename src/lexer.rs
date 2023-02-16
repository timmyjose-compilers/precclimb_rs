#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Tint(i32),
    Tlp,
    Trp,
    Tplus,
    Tminus,
    Tmult,
    Tdiv,
    Tmod,
    Tend,
}

impl Token {
    pub fn is_binary_op(&self) -> bool {
        match *self {
            Token::Tplus | Token::Tminus | Token::Tmult | Token::Tdiv | Token::Tmod => true,
            _ => false,
        }
    }

    pub fn precedence(&self) -> i32 {
        match *self {
            Token::Tmod => 10,
            Token::Tplus | Token::Tminus => 20,
            Token::Tmult | Token::Tdiv => 30,
            _ => MIN_PRECEDENCE,
        }
    }

    pub fn is_right_associative(&self) -> bool {
        match *self {
            _ => false,
        }
    }
}

pub const MIN_PRECEDENCE: i32 = 0;
pub const MAX_PRECEDENCE: i32 = 100;

#[derive(Debug)]
pub enum LexerError {
    NoMoreCharacters,
    InvalidCharacter(char),
    WrappedError(Box<dyn std::error::Error>),
}

impl std::error::Error for LexerError {}

use std::fmt;
impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                LexerError::NoMoreCharacters => "no more characters in the input stream".into(),
                LexerError::InvalidCharacter(c) => format!("invalid character while lexing: {c}"),
                LexerError::WrappedError(ref err) => err.to_string(),
            }
        )
    }
}

impl From<std::num::ParseIntError> for LexerError {
    fn from(pie: std::num::ParseIntError) -> Self {
        LexerError::WrappedError(Box::new(pie))
    }
}

pub type LexerResult<T> = Result<T, LexerError>;

pub struct Lexer {
    src: String,
    curr_pos: usize,
    size: usize,
}

impl Lexer {
    pub fn new(src: &str) -> Self {
        let size = src.len();

        Lexer {
            src: src.to_owned(),
            curr_pos: 0,
            size,
        }
    }

    fn forward(&mut self) {
        self.curr_pos += 1;
    }

    fn backward_n(&mut self, n: usize) {
        self.curr_pos -= n;
    }

    fn extract_pred<P>(&mut self, pred: P) -> LexerResult<(usize, &str)>
    where
        P: Fn(char) -> bool,
    {
        let start_pos = self.curr_pos;
        let mut running_pos = self.curr_pos;

        while running_pos < self.size
            && pred(
                self.src
                    .chars()
                    .nth(running_pos)
                    .ok_or(LexerError::NoMoreCharacters)?,
            )
        {
            running_pos += 1;
        }

        self.curr_pos = running_pos;
        Ok((running_pos - start_pos, &self.src[start_pos..running_pos]))
    }

    fn extract_int(&mut self) -> LexerResult<(usize, i32)> {
        let (npos, intstr) = self.extract_pred(|c| c.is_digit(10))?;
        Ok((npos, intstr.parse::<i32>()?))
    }

    fn lex_char(&mut self, c: char, consume: bool) -> LexerResult<Token> {
        use Token::*;

        Ok(match c {
            c if c.is_whitespace() => {
                self.forward();
                if consume {
                    self.lex()?
                } else {
                    self.peek()?
                }
            }

            c if c.is_digit(10) => {
                let (npos, int) = self.extract_int()?;
                if consume {
                    Tint(int)
                } else {
                    self.backward_n(npos);
                    Tint(int)
                }
            }

            '(' => {
                if consume {
                    self.forward();
                }
                Tlp
            }

            ')' => {
                if consume {
                    self.forward();
                }
                Trp
            }

            '+' => {
                if consume {
                    self.forward();
                }
                Tplus
            }

            '-' => {
                if consume {
                    self.forward();
                }
                Tminus
            }
            '*' => {
                if consume {
                    self.forward();
                }
                Tmult
            }
            '/' => {
                if consume {
                    self.forward();
                }
                Tdiv
            }
            '%' => {
                if consume {
                    self.forward();
                }
                Tmod
            }
            _ => return Err(LexerError::InvalidCharacter(c)),
        })
    }

    fn peek_or_lex(&mut self, consume: bool) -> LexerResult<Token> {
        if self.curr_pos >= self.size {
            Ok(Token::Tend)
        } else {
            self.lex_char(
                self.src
                    .chars()
                    .nth(self.curr_pos)
                    .ok_or(LexerError::NoMoreCharacters)?,
                consume,
            )
        }
    }

    pub fn peek(&mut self) -> LexerResult<Token> {
        self.peek_or_lex(false)
    }

    pub fn lex(&mut self) -> LexerResult<Token> {
        self.peek_or_lex(true)
    }
}

#[cfg(test)]
mod tests {
    use super::{Lexer, Token::*};
    use std::error::Error;

    #[test]
    fn test_lex() -> Result<(), Box<dyn Error>> {
        let expected_tokens = vec![Tint(1), Tplus, Tint(2), Tmult, Tint(3), Tend];

        let mut lexer = Lexer::new("1 + 2 * 3");
        let mut actual_tokens = Vec::new();
        loop {
            let tok = lexer.lex()?;
            actual_tokens.push(tok.clone());
            if tok == Tend {
                break;
            }
        }
        assert_eq!(expected_tokens, actual_tokens);
        Ok(())
    }

    #[test]
    fn test_peek() -> Result<(), Box<dyn Error>> {
        let expected_tokens = vec![
            Tint(1),
            Tint(1),
            Tint(1),
            Tint(1),
            Tplus,
            Tplus,
            Tint(2),
            Tmult,
            Tmult,
            Tint(3),
            Tend,
            Tend,
            Tend,
        ];

        let mut lexer = Lexer::new("1 + 2 * 3");
        let mut actual_tokens = Vec::new();
        actual_tokens.push(lexer.peek()?); // 1
        actual_tokens.push(lexer.peek()?); // 1
        actual_tokens.push(lexer.peek()?); // 1
        actual_tokens.push(lexer.lex()?); // 1
        actual_tokens.push(lexer.peek()?); // +
        actual_tokens.push(lexer.lex()?); // +
        actual_tokens.push(lexer.lex()?); // 2
        actual_tokens.push(lexer.peek()?); // *
        actual_tokens.push(lexer.lex()?); // *
        actual_tokens.push(lexer.lex()?); // 3
        actual_tokens.push(lexer.lex()?); // <eof>
        actual_tokens.push(lexer.peek()?); // <eof>
        actual_tokens.push(lexer.peek()?); // <eof>

        assert_eq!(expected_tokens, actual_tokens);
        Ok(())
    }
}
