#[derive(Debug, PartialEq)]
pub enum UnaryOp {
    Uplus,
    Uminus,
}

#[derive(Debug, PartialEq)]
pub enum BinaryOp {
    Plus,
    Minus,
    Mult,
    Div,
    Mod,
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    ExpInt(i32),
    ExpUnary {
        op: UnaryOp,
        exp: Box<Expression>,
    },
    ExpBinary {
        exp1: Box<Expression>,
        op: BinaryOp,
        exp2: Box<Expression>,
    },
}
