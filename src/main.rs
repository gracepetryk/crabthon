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
    RightParen
}

#[derive(Debug)]
enum Expr {
    Numeral(i32),
    Sum(SumOperation, Box<Expr>, Box<Expr>),
    Term(TermOperation, Box<Expr>, Box<Expr>)
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
        let label = match self {
            Expr::Numeral(v) => v.to_string(),
            Expr::Sum(SumOperation::ADD, _, _) => "+".to_string(),
            Expr::Sum(SumOperation::SUBTRACT, _, _) => "-".to_string(),
            Expr::Term(TermOperation::MULTIPLY, _, _) => "*".to_string(),
            Expr::Term(TermOperation::DIVIDE, _, _) => "/".to_string(),
        };

        println!("\"{}\" [label=\"{}\"]; ", node_count, label);

        match self {
            Expr::Sum(_, e1, e2) | Expr::Term(_, e1, e2) => {
                e1._fmt(f, node_count*2 + 1)?;
                e2._fmt(f, node_count*2 + 2)?;

                println!("\"{}\" -> \"{}\"; ", node_count, 2 * node_count + 1);
                println!("\"{}\" -> \"{}\"; ", node_count, 2 * node_count + 2);

            },
            _ => ()
        }

        Ok(())
    }
}

#[derive(Debug)]
enum SumOperation {
    ADD,
    SUBTRACT
}

#[derive(Debug)]
enum TermOperation {
    MULTIPLY,
    DIVIDE
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
        return self.parse_sum()
    }


    fn parse_sum(&self) -> ParseResult<Expr> {
        let lh_term = self.parse_term()?;

        let operation = match self.peek() {
            Some(Token::PLUS) => SumOperation::ADD,
            Some(Token::MINUS) => SumOperation::SUBTRACT,
            _ => { return Ok(lh_term) }
        };

        self.next();
        return Ok(Expr::Sum(operation, Box::new(lh_term), Box::new(self.parse_sum()?)))
    }

    fn parse_term(&self) -> ParseResult<Expr> {
        let primary = self.parse_primary()?;

        let operation = match self.peek() {
            Some(Token::STAR) => TermOperation::MULTIPLY,
            Some(Token::SLASH) => TermOperation::DIVIDE,
            _ => { return Ok(primary) }
        };

        self.next();
        return Ok(Expr::Term(operation, Box::new(primary), Box::new(self.parse_term()?)))

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

fn eval_expr(e: &Expr) -> i32 {
    return match e {
        Expr::Numeral(n) => *n,
        Expr::Term(TermOperation::MULTIPLY, a , b)  => eval_expr(a) * eval_expr(b),
        Expr::Term(TermOperation::DIVIDE,a , b)     => eval_expr(a) / eval_expr(b),
        Expr::Sum(SumOperation::ADD, a, b)          => eval_expr(a) + eval_expr(b),
        Expr::Sum(SumOperation::SUBTRACT, a, b)     => eval_expr(a) - eval_expr(b),
    }
}

fn main() {
    let expr = Parser::new(vec![
        Token::Literal(1),
        Token::STAR,
        Token::Literal(2),

        Token::PLUS,

        Token::Literal(3),
        Token::STAR,
        Token::Literal(4),
    ]).parse().unwrap();

    // println!("{:?}", expr);
    println!("{:}", expr);
    // println!("{:?}", eval_expr(&expr));
}
