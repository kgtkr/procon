fn main() {
    println!("Hello, world!");
}

#[derive(Eq, PartialEq, Debug)]
enum AST {
    Number(i64),
    Pow(Box<AST>, Box<AST>),
}

struct Parser {
    s: Vec<char>,
    pos: usize,
}

impl Parser {
    pub fn new(s: String) -> Parser {
        Parser {
            s: s.chars().collect(),
            pos: 0,
        }
    }

    fn peek(&self) -> Option<char> {
        self.s.get(self.pos).cloned()
    }

    fn next(&mut self) -> Option<char> {
        let val = self.peek();
        self.pos += 1;
        val
    }

    fn char(&mut self, c: char) -> Option<char> {
        self.expect(|x| x == c)
    }

    fn expect<F>(&mut self, f: F) -> Option<char>
    where
        F: FnOnce(char) -> bool,
    {
        match self.peek() {
            Some(x) if f(x) => {
                self.next();
                Some(x)
            }
            _ => None,
        }
    }

    fn number_prefix(&mut self) -> Option<i64> {
        let val = self.peek()?;
        let res = match val {
            '〇' => 0,
            '一' => 1,
            '二' => 2,
            '三' => 3,
            '四' => 4,
            '五' => 5,
            '六' => 6,
            '七' => 7,
            '八' => 8,
            '九' => 9,
            _ => return None,
        };
        self.next()?;
        Some(res)
    }

    fn number_suffix_min(&mut self) -> Option<i64> {
        let val = self.peek()?;
        let res = match val {
            '十' => 10,
            '百' => 100,
            '千' => 1000,
            _ => return None,
        };
        self.next()?;
        Some(res)
    }

    fn number_term_min(&mut self) -> Option<i64> {
        match (self.number_prefix(), self.number_suffix_min()) {
            (Some(p), Some(s)) => Some(p * s),
            (Some(p), None) => Some(p),
            (None, Some(s)) => Some(s),
            _ => None,
        }
    }

    fn number_suffix_big(&mut self) -> Option<i64> {
        let val = self.peek()?;
        let res = match val {
            '万' => 10000,
            '億' => 100000000,
            _ => return None,
        };
        self.next()?;
        Some(res)
    }

    fn number_term(&mut self) -> Option<i64> {
        match (self.number_term_min(), self.number_suffix_big()) {
            (Some(p), Some(s)) => Some(p * s),
            (Some(p), None) => Some(p),
            (None, Some(s)) => Some(s),
            _ => None,
        }
    }

    fn number(&mut self) -> Option<i64> {
        let mut x = self.number_term()?;
        while let Some(y) = self.number_term() {
            x += y;
        }
        Some(x)
    }

    fn expr_pow(&mut self) -> Option<AST> {
        self.char('の')?;
        let x = self.expr()?;
        self.char('乗')?;
        Some(x)
    }
    fn expr(&mut self) -> Option<AST> {
        let mut x = AST::Number(self.number()?);
        while let Some(a) = self.expr_pow() {
            x = AST::Pow(Box::new(x), Box::new(a));
        }
        Some(x)
    }

    fn eof(&self) -> Option<()> {
        if self.s.len() == self.pos {
            Some(())
        } else {
            None
        }
    }
}

fn parse(s: String) -> AST {
    Parser::new(s).expr().unwrap()
}

#[test]
fn parse_test() {
    assert_eq!(
        parse("四の三の二乗乗".to_string()),
        AST::Pow(
            Box::new(AST::Number(4)),
            Box::new(AST::Pow(Box::new(AST::Number(3)), Box::new(AST::Number(2))))
        )
    );

    assert_eq!(
        parse("四の三乗の二乗".to_string()),
        AST::Pow(
            Box::new(AST::Pow(Box::new(AST::Number(4)), Box::new(AST::Number(3)))),
            Box::new(AST::Number(2)),
        )
    );

    assert_eq!(
        parse("一億二千三百四十五万六千七百八十九".to_string()),
        AST::Number(123456789)
    );
}
