use crate::{
    ast::{BinaryOp, Expression, UnaryOp},
    lexer::{self, Lexer, LexerError, Token},
};

#[derive(Debug)]
pub enum ParserError {
    LexerError(LexerError),
    MissingParenthesis,
    InvalidUnaryOp(Token),
    InvalidBinaryOp(Token),
    InvalidPrimaryToken(Token),
}

impl std::error::Error for ParserError {}

use std::fmt;

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                ParserError::LexerError(ref lerr) => lerr.to_string(),
                ParserError::MissingParenthesis =>
                    "missing parenthesis while parsing expression".into(),
                ParserError::InvalidUnaryOp(ref tok) =>
                    format!("Invalid token for unary operator: {tok:#?}"),
                ParserError::InvalidBinaryOp(ref tok) =>
                    format!("Invalid token for binary operator: {tok:#?}"),
                ParserError::InvalidPrimaryToken(ref tok) =>
                    format!("invalid token for primary expression: {tok:#?}"),
            }
        )
    }
}

pub type ParserResult<T> = Result<T, ParserError>;

pub struct Parser {
    lexer: Lexer,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        Parser { lexer }
    }

    fn get_unary_op(&self, tok: &Token) -> ParserResult<UnaryOp> {
        Ok(match *tok {
            Token::Tplus => UnaryOp::Uplus,
            Token::Tminus => UnaryOp::Uminus,
            _ => return Err(ParserError::InvalidUnaryOp(tok.clone())),
        })
    }

    fn get_binary_op(&self, tok: &Token) -> ParserResult<BinaryOp> {
        Ok(match *tok {
            Token::Tplus => BinaryOp::Plus,
            Token::Tminus => BinaryOp::Minus,
            Token::Tmult => BinaryOp::Mult,
            Token::Tdiv => BinaryOp::Div,
            Token::Tmod => BinaryOp::Mod,
            _ => return Err(ParserError::InvalidBinaryOp(tok.clone())),
        })
    }

    fn peek(&mut self) -> ParserResult<Token> {
        Ok(self
            .lexer
            .peek()
            .or_else(|lerr| Err(ParserError::LexerError(lerr)))?)
    }

    fn lex(&mut self) -> ParserResult<Token> {
        Ok(self
            .lexer
            .lex()
            .or_else(|lerr| Err(ParserError::LexerError(lerr)))?)
    }

    fn parse_primary(&mut self) -> ParserResult<Expression> {
        let tok = self.lex()?;

        Ok(match tok {
            Token::Tint(n) => Expression::ExpInt(n),
            Token::Tlp => {
                let expr = self.parse_expression()?;

                if self.peek()? != Token::Trp {
                    return Err(ParserError::MissingParenthesis);
                }

                let _ = self.lex()?;
                expr
            }

            Token::Tplus | Token::Tminus => {
                let prim = self.parse_primary()?;
                Expression::ExpUnary {
                    op: self.get_unary_op(&tok)?,
                    exp: Box::new(self.parse_expression_aux(prim, lexer::MAX_PRECEDENCE)?),
                }
            }
            _ => return Err(ParserError::InvalidPrimaryToken(tok.clone())),
        })
    }

    fn parse_expression_aux(
        &mut self,
        mut lhs: Expression,
        min_precedence: i32,
    ) -> ParserResult<Expression> {
        let mut lookahead = self.peek()?;

        while lookahead.is_binary_op() && lookahead.precedence() >= min_precedence {
            let op = lookahead;
            self.lex()?;
            let mut rhs = self.parse_primary()?;
            lookahead = self.peek()?;

            while lookahead.is_binary_op()
                && (lookahead.precedence() > op.precedence()
                    || lookahead.is_right_associative()
                        && lookahead.precedence() == op.precedence())
            {
                rhs = self.parse_expression_aux(rhs, lookahead.precedence())?;
                lookahead = self.peek()?;
            }

            lhs = Expression::ExpBinary {
                exp1: Box::new(lhs),
                op: self.get_binary_op(&op)?,
                exp2: Box::new(rhs),
            };
        }
        Ok(lhs)
    }

    fn parse_expression(&mut self) -> ParserResult<Expression> {
        let prim = self.parse_primary()?;
        self.parse_expression_aux(prim, lexer::MIN_PRECEDENCE)
    }

    pub fn parse(&mut self) -> ParserResult<Expression> {
        self.parse_expression()
    }
}

#[cfg(test)]
mod tests {
    use super::Parser;
    use crate::{
        ast::{BinaryOp::*, Expression::*, UnaryOp::*},
        lexer::Lexer,
    };
    use std::error::Error;

    #[test]
    fn test_parse_int() -> Result<(), Box<dyn Error>> {
        let mut parser = Parser::new(Lexer::new("12345"));
        let ast = parser.parse()?;
        assert_eq!(ast, ExpInt(12345));
        Ok(())
    }

    #[test]
    fn test_parse_expression_1() -> Result<(), Box<dyn Error>> {
        let mut parser = Parser::new(Lexer::new("1 + 2 * 3"));
        let ast = parser.parse()?;
        assert_eq!(
            ast,
            ExpBinary {
                exp1: Box::new(ExpInt(1)),
                op: Plus,
                exp2: Box::new(ExpBinary {
                    exp1: Box::new(ExpInt(2)),
                    op: Mult,
                    exp2: Box::new(ExpInt(3))
                })
            }
        );

        Ok(())
    }

    #[test]
    fn test_parse_expression_2() -> Result<(), Box<dyn Error>> {
        let mut parser = Parser::new(Lexer::new("1 * 2 + 3"));
        let ast = parser.parse()?;
        assert_eq!(
            ast,
            ExpBinary {
                exp1: Box::new(ExpBinary {
                    exp1: Box::new(ExpInt(1)),
                    op: Mult,
                    exp2: Box::new(ExpInt(2))
                }),
                op: Plus,
                exp2: Box::new(ExpInt(3))
            }
        );

        Ok(())
    }

    #[test]
    fn test_parse_unary_expression_1() -> Result<(), Box<dyn Error>> {
        let mut parser = Parser::new(Lexer::new("2 - -1"));
        let ast = parser.parse()?;
        assert_eq!(
            ast,
            ExpBinary {
                exp1: Box::new(ExpInt(2)),
                op: Minus,
                exp2: Box::new(ExpUnary {
                    op: Uminus,
                    exp: Box::new(ExpInt(1))
                })
            }
        );

        Ok(())
    }
}
