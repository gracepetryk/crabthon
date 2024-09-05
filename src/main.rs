use std::cell::RefCell;

#[derive(PartialEq, Debug)]
enum TokenType {
    Number,
    Operator
}

#[derive(Debug)]
struct Token<'a> {
    typ: TokenType,
    val: &'a str,
}

#[derive(Debug)]
enum Expr {
    Numeral(u32),
    Sum(Box<Expr>, Box<Expr>),
    Term(Box<Expr>, Box<Expr>)
}


struct SumExpr {
    left: Box<Expr>,
    right: Box<Expr>
}

struct Parser<'a> {
    tokens: Vec<Token<'a>>,
    pos: RefCell<usize>
}

type ParseResult<T> = Result<T, &'static str>;

impl<'a> Parser<'a> {
    fn new(tokens: Vec<Token<'a>>) -> Self {
        Self { tokens, pos: RefCell::new(0) }
    }

    fn is_eof(&self) -> bool {
        return *self.pos.borrow() >= self.tokens.len()
    }

    fn peek(&self) -> Option<&Token> {
        return (! self.is_eof()).then(|| &self.tokens[*self.pos.borrow()])
    }

    fn consume(&mut self) -> Option<&Token> {
        let tok = self.peek();
        self.next();
        return tok
    }

    fn next(&self) {
        self.pos.replace_with(|&mut old| old + 1);
    }

    fn parse(&mut self) -> ParseResult<Expr> {
        return self.parse_sum()
    }


    fn parse_sum(&mut self) -> ParseResult<Expr> {
        let lhs = self.parse_term()?;
        self.parse_binexpr(lhs, "+")
    }

    fn parse_term(&mut self) -> ParseResult<Expr> {
        let lhs = self.parse_numeral()?;
        self.parse_binexpr(lhs, "*")
    }

    fn parse_numeral(&mut self) -> ParseResult<Expr> {
        let tok = self.consume().ok_or("No Token!")?;

        if tok.typ != TokenType::Number {
            return Err("Token was not a number!");
        };

        return Ok(Expr::Numeral(tok.val.parse().map_err(|_| "Invalid numeral.")?));
    }

    fn build_binexpr(&mut self, operator: &'a str, lhs: Expr) -> ParseResult<Expr> {
        match operator {
            "*" => Ok(Expr::Term(Box::new(lhs), Box::new(self.parse_term()?))),
            "+" => Ok(Expr::Sum(Box::new(lhs), Box::new(self.parse_sum()?))),
            _ => Err("Unrecognized operator!")
        }
    }

    fn parse_binexpr(&mut self, lhs: Expr, operator: &'a str) -> ParseResult<Expr> {
        let op_maybe = self.peek();

        let expr = match op_maybe {
            Some(Token{ typ: TokenType::Operator, val: op }) if *op == operator => {
                self.next();
                self.build_binexpr(operator, lhs)?
            },
            None | _ => lhs
        };

        return Ok(expr)
    }
}

fn eval_expr(e: &Expr) -> u32 {
    return match e {
        Expr::Numeral(n) => *n,
        Expr::Term(a, b) => eval_expr(a) * eval_expr(b),
        Expr::Sum(a, b) => eval_expr(a) + eval_expr(b),
    }
}

fn main() {
    let expr = Parser::new(vec![
        Token{ typ: TokenType::Number, val: "1" },
        Token{ typ: TokenType::Operator, val: "+" },
        Token{ typ: TokenType::Number, val: "2" },
        Token{ typ: TokenType::Operator, val: "*" },
        Token{ typ: TokenType::Number, val: "3" },
    ]).parse().unwrap();

    println!("{:?}", expr);
    println!("{:?}", eval_expr(&expr));
}
