use std::{cell::RefCell, fmt::{self}};

#[derive(PartialEq, Debug)]
#[allow(non_camel_case_types)]
enum Token {
    Literal(i32),
    STAR,
    SLASH,
    PLUS,
    MINUS,
    LeftParen,
    RightParen,

    OR,
    AND,
    NOT,

    EQ,
    NEQ,
    LT,
    LTE,
    GT,
    GTE
}

#[derive(Debug)]
enum BinaryExprKind {
    Add,
    Subtract,
    Multiply,
    Divide,

    OR,
    AND,

    EQ,
    NEQ,
    LT,
    LTE,
    GT,
    GTE

}

#[derive(Debug)]
enum Expr {
    Numeral(i32),
    Not(Box<Expr>),
    BinaryExpr { kind: BinaryExprKind, left: Box<Expr>, right: Box<Expr>},
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "digraph {{")?;
        self._fmt(f, 0)?;
        write!(f, "}}")
    }
}

impl Expr {
    fn _fmt(&self, f: &mut fmt::Formatter, node_count: u32) -> fmt::Result {
        use Expr::*;
        use BinaryExprKind::*;

        let label = match self {
            Numeral(v) => v.to_string(),
            Not(_) => "not".to_string(),

            BinaryExpr{ kind: Add, ..} => "+".to_string(),
            BinaryExpr{ kind: Subtract, ..} => "-".to_string(),
            BinaryExpr{ kind: Multiply, ..} => "*".to_string(),
            BinaryExpr{ kind: Divide, ..} => "/".to_string(),

            BinaryExpr{ kind: OR, ..} => "or".to_string(),
            BinaryExpr{ kind: AND, ..} => "and".to_string(),


            BinaryExpr{ kind: EQ, ..} => "==".to_string(),
            BinaryExpr{ kind: NEQ, ..} => "!=".to_string(),
            BinaryExpr{ kind: LT, ..} => "<".to_string(),
            BinaryExpr{ kind: LTE, ..} => "<=".to_string(),
            BinaryExpr{ kind: GT, ..} => ">".to_string(),
            BinaryExpr{ kind: GTE, ..} => ">=".to_string(),
        };

        println!("\"{}\" [label=\"{}\"]; ", node_count, label);

        match self {
            Expr::BinaryExpr { left, right, .. } => {
                left._fmt(f, node_count*2 + 1)?;
                right._fmt(f, node_count*2 + 2)?;

                write!(f, "\"{}\" -> \"{}\"; ", node_count, 2 * node_count + 1)?;
                write!(f, "\"{}\" -> \"{}\"; ", node_count, 2 * node_count + 2)?;

            },
            Expr::Not(exp) => {
                exp._fmt(f, 2 * node_count + 1)?;
                write!(f, "\"{}\" -> \"{}\"; ", node_count, 2 * node_count + 1)?;
            }
            _ => ()
        }

        Ok(())
    }
}

struct Parser {
    tokens: Vec<Token>,
    pos: RefCell<usize>
}

#[derive(Debug)]
enum ParseError<'a> {
    UnexpectedToken(&'a str, &'a Token),
    MissingToken(Token),
    ExpectedLiteral,
    UnexpectedEOF,
    LiteralNegation,
}

type ParseResult<'a, T> = Result<T, ParseError<'a>>;

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: RefCell::new(0) }
    }

    fn is_eof(&self) -> bool {
        return *self.pos.borrow() >= self.tokens.len()
    }

    fn peek(&self) -> Option<&Token> {
        return (! self.is_eof()).then(|| &self.tokens[*self.pos.borrow()])
    }

    fn consume(&self) -> Option<&Token> {
        let tok = self.peek();
        self.next();
        return tok
    }

    fn next(&self) {
        self.pos.replace_with(|&mut old| old + 1);
    }

    fn parse(&self) -> ParseResult<Expr> {
        return self.parse_disjunction()
    }

    fn parse_disjunction(&self) -> ParseResult<Expr> {
        let lh_term = self.parse_conjunction()?;

        let operation = match self.peek() {
            Some(Token::OR) => BinaryExprKind::OR,
            _ => { return Ok(lh_term) }
        };

        self.next();
        return Ok(Expr::BinaryExpr {
            kind: operation,
            left: Box::new(lh_term),
            right: Box::new(self.parse_disjunction()?)
        })
    }

    fn parse_conjunction(&self) -> ParseResult<Expr> {
        let lh_term = self.parse_inversion()?;

        let operation = match self.peek() {
            Some(Token::AND) => BinaryExprKind::AND,
            _ => { return Ok(lh_term) }
        };

        self.next();
        return Ok(Expr::BinaryExpr {
            kind: operation,
            left: Box::new(lh_term),
            right: Box::new(self.parse_inversion()?)
        })
    }

    fn parse_inversion(&self) -> ParseResult<Expr> {
        match self.peek() {
            Some(Token::NOT) => {
                self.next();
                return match self.peek() {
                    Some(Token::Literal(_)) => Err(ParseError::LiteralNegation),
                    _ => Ok(Expr::Not(Box::new(self.parse_inversion()?)))
                }
            },
            Some(_) => return Ok(self.parse_comparison()?),
            None => return Err(ParseError::UnexpectedEOF)
        }
    }

    fn parse_comparison(&self) -> ParseResult<Expr> {
        let lh_term = self.parse_sum()?;

        let operation = match self.peek() {
            Some(Token::EQ) => BinaryExprKind::EQ,
            Some(Token::NEQ) => BinaryExprKind::NEQ,
            Some(Token::LT) => BinaryExprKind::LT,
            Some(Token::LTE) => BinaryExprKind::LTE,
            Some(Token::GT) => BinaryExprKind::GT,
            Some(Token::GTE) => BinaryExprKind::GTE,
            _ => { return Ok(lh_term) }
        };

        self.next();
        return Ok(Expr::BinaryExpr {
            kind: operation,
            left: Box::new(lh_term),
            right: Box::new(self.parse_comparison()?)
        })
    }


    fn parse_sum(&self) -> ParseResult<Expr> {
        let lh_term = self.parse_term()?;

        let operation = match self.peek() {
            Some(Token::PLUS) => BinaryExprKind::Add,
            Some(Token::MINUS) => BinaryExprKind::Subtract,
            _ => { return Ok(lh_term) }
        };

        self.next();
        return Ok(Expr::BinaryExpr {
            kind: operation,
            left: Box::new(lh_term),
            right: Box::new(self.parse_sum()?)
        })
    }

    fn parse_term(&self) -> ParseResult<Expr> {
        let primary = self.parse_primary()?;

        let operation = match self.peek() {
            Some(Token::STAR) => BinaryExprKind::Multiply,
            Some(Token::SLASH) => BinaryExprKind::Divide,
            _ => { return Ok(primary) }
        };

        self.next();
        return Ok(Expr::BinaryExpr {
            kind: operation,
            left: Box::new(primary),
            right: Box::new(self.parse_term()?)
        })

    }

    fn parse_primary(&self) -> ParseResult<Expr> {
        let tok = self.consume();

        return match tok {
            Some(Token::LeftParen) => {
                let expr = self.parse()?;
                match self.consume() {
                    Some(Token::RightParen) => return Ok(expr),
                    Some(t) => return Err(ParseError::UnexpectedToken(")", t)),
                    _ => return Err(ParseError::MissingToken(Token::RightParen))
                }
            },
            Some(Token::Literal(v)) => Ok(Expr::Numeral(*v)),
            None => Err(ParseError::ExpectedLiteral),
            Some(t) => Err(ParseError::UnexpectedToken("Literal value", t))
        }
    }

}

#[derive(Debug)]
enum RuntimeError {
    InvalidOperands,
    InvalidType,
}

#[derive(Debug)]
enum ExprOutput {
    Int(i32),
    Bool(bool),
}

type ExprResult = Result<ExprOutput, RuntimeError>;

fn eval_expr(e: &Expr) -> ExprResult {
    use BinaryExprKind::*;
    use ExprOutput::*;

    return match e {
        Expr::Numeral(n) => Ok(Int(*n)),
        Expr::Not(exp) => {
            let val = eval_expr(exp)?;

            match val {
                Bool(v) => return Ok(Bool(! v)),
                _ => return Err(RuntimeError::InvalidType)
            }
        },
        Expr::BinaryExpr { kind, left, right } => {
            let l_val = eval_expr(left)?;

            // short-circuiting
            match (kind, &l_val) {
                (OR, Bool(true)) => return Ok(Bool(true)),
                (AND, Bool(false)) => return Ok(Bool(false)),
                _ => ()
            };

            let r_val = eval_expr(right)?;

            return Ok(match (kind, l_val, r_val) {
                (Add, Int(l), Int(r)) => Int(l + r),
                (Subtract, Int(l), Int(r)) => Int(l - r),
                (Multiply, Int(l), Int(r)) => Int(l * r),
                (Divide, Int(l), Int(r)) => Int(l / r),

                (OR, Bool(_), Bool(r)) => Bool(r),  // we already checked the left expr during
                                                    // short circuit evaluation.
                (AND, Bool(_), Bool(r)) => Bool(r),

                (EQ, Int(l), Int(r)) => Bool(l == r),
                (EQ, Bool(l), Bool(r)) => Bool(l == r),
                (NEQ, Int(l), Int(r)) => Bool(l != r),
                (NEQ, Bool(l), Bool(r)) => Bool(l != r),

                (LT, Int(l), Int(r)) => Bool(l < r),
                (LTE, Int(l), Int(r)) => Bool(l <= r),
                (GT, Int(l), Int(r)) => Bool(l > r),
                (GTE, Int(l), Int(r)) => Bool(l >= r),

                (_, _, _) => return Err(RuntimeError::InvalidOperands)
            })
        }
    }
}

fn main() {
    use Token as T;

    let expr = Parser::new(vec![
        T::NOT,
        T::LeftParen,
        T::Literal(10),
        T::PLUS,
        T::Literal(2),
        T::MINUS,
        T::Literal(12),
        T::GTE,
        T::Literal(10),
        T::SLASH,
        T::Literal(13),
        T::STAR,
        T::Literal(2),
        T::AND,
        T::Literal(1),
        T::EQ,
        T::Literal(1),
        T::RightParen
    ]).parse().unwrap();

    // println!("{:?}", expr);
    println!("{:}", expr);
    println!("/* {:?} */", eval_expr(&expr).unwrap());
}
