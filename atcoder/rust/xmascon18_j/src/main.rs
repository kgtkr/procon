extern crate core;

use std::io::{self, Read};

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = solve(input.trim().to_string());
    println!("{}", output);
}

fn solve(s: String) -> String {
    let mut memo = HashMap::new();
    num_to_string::num(eval(Parser::parse(s), M, &mut memo))
}

macro_rules! tests {
    ($($name:ident: $input:expr=>$output:expr,)*) => {
        mod tests {
            $(
                #[test]
                fn $name() {
                    assert_eq!($output.trim().to_string(),super::solve($input.trim().to_string()));
                }
            )*
        }
    }
}

/*
原始的な:primitive→pri
小さい:low
大きい:high
係数:coefficient→coe
累乗数:perfect power→pp
項:term
数字:number→num

命名
二:pri
十:low_pp
万:high_pp
三百:low_term
十万:term
千三百:low_num
一億五千万:num
*/

const M: i64 = 1000000009;

use std::collections::HashMap;

fn memo_phi(n: i64, memo: &mut HashMap<i64, i64>) -> i64 {
    if let Some(&x) = memo.get(&n) {
        x
    } else {
        let res = phi(n);
        memo.insert(n, res);
        res
    }
}

fn phi(mut n: i64) -> i64 {
    if n == 0 {
        0
    } else {
        let mut ans = n;
        let mut x = 2;

        while x * x <= n {
            if n % x == 0 {
                ans -= ans / x;
                while n % x == 0 {
                    n /= x;
                }
            }
            x += 1;
        }

        if n > 1 {
            ans -= ans / n;
        }
        ans
    }
}

fn mod_pow(a: i64, b: i64, m: i64) -> i64 {
    (a % m).pow((b % phi(m)) as u32)
}

fn eval(ast: AST, m: i64, memo: &mut HashMap<i64, i64>) -> i64 {
    match ast {
        AST::Num(x) => x % m,
        AST::Pow(a, b) => eval(*a, m, memo).pow(eval(*b, memo_phi(m, memo), memo) as u32) % m,
    }
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum AST {
    Num(i64),
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

    fn peek(&self) -> Result<char, ()> {
        self.s.get(self.pos).cloned().ok_or(())
    }

    fn next(&mut self) -> Result<char, ()> {
        let val = self.peek();
        self.pos += 1;
        val
    }

    fn char(&mut self, c: char) -> Result<char, ()> {
        self.expect(|x| x == c)
    }

    fn expect<F>(&mut self, f: F) -> Result<char, ()>
    where
        F: FnOnce(char) -> bool,
    {
        match self.peek() {
            Ok(x) if f(x) => {
                self.next();
                Ok(x)
            }
            _ => Err(()),
        }
    }

    fn pri(&mut self) -> Result<i64, ()> {
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
            _ => return Err(()),
        };
        self.next()?;
        Ok(res)
    }

    fn low_pp(&mut self) -> Result<i64, ()> {
        let val = self.peek()?;
        let res = match val {
            '十' => 10,
            '百' => 100,
            '千' => 1000,
            _ => return Err(()),
        };
        self.next()?;
        Ok(res)
    }

    fn low_term(&mut self) -> Result<i64, ()> {
        match (self.pri(), self.low_pp()) {
            (Ok(p), Ok(s)) => Ok(p * s),
            (Ok(p), Err(_)) => Ok(p),
            (Err(_), Ok(s)) => Ok(s),
            _ => Err(()),
        }
    }

    fn low_num(&mut self) -> Result<i64, ()> {
        let mut x = self.low_term()?;
        while let Ok(y) = self.low_term() {
            x += y;
        }
        Ok(x)
    }

    fn high_pp(&mut self) -> Result<i64, ()> {
        let val = self.peek()?;
        let res = match val {
            '万' => 10000,
            '億' => 100000000,
            _ => return Err(()),
        };
        self.next()?;
        Ok(res)
    }

    fn term(&mut self) -> Result<i64, ()> {
        match (self.low_num(), self.high_pp()) {
            (Ok(p), Ok(s)) => Ok(p * s),
            (Ok(p), Err(_)) => Ok(p),
            (Err(_), Ok(s)) => Ok(s),
            _ => Err(()),
        }
    }

    fn num(&mut self) -> Result<i64, ()> {
        let mut x = self.term()?;
        while let Ok(y) = self.term() {
            x += y;
        }
        Ok(x)
    }

    fn expr_pow(&mut self) -> Result<AST, ()> {
        self.char('の')?;
        let x = self.expr()?;
        self.char('乗')?;
        Ok(x)
    }
    fn expr(&mut self) -> Result<AST, ()> {
        let mut x = AST::Num(self.num()?);
        while let Ok(a) = self.expr_pow() {
            x = AST::Pow(Box::new(x), Box::new(a));
        }
        Ok(x)
    }

    fn eof(&self) -> Result<(), ()> {
        if self.s.len() == self.pos {
            Ok(())
        } else {
            Err(())
        }
    }

    fn parse(s: String) -> AST {
        Parser::new(s).expr().unwrap()
    }
}

#[test]
fn parse_test() {
    assert_eq!(
        Parser::parse("四の三の二乗乗".to_string()),
        AST::Pow(
            Box::new(AST::Num(4)),
            Box::new(AST::Pow(Box::new(AST::Num(3)), Box::new(AST::Num(2))))
        )
    );

    assert_eq!(
        Parser::parse("四の三乗の二乗".to_string()),
        AST::Pow(
            Box::new(AST::Pow(Box::new(AST::Num(4)), Box::new(AST::Num(3)))),
            Box::new(AST::Num(2)),
        )
    );

    assert_eq!(Parser::parse("一億".to_string()), AST::Num(100000000));
    assert_eq!(
        Parser::parse("三十二億".to_string()),
        AST::Num(3200000000)
    );

    assert_eq!(
        Parser::parse("一億二千三百四十五万六千七百八十九".to_string()),
        AST::Num(123456789)
    );
}

mod num_to_string {
    //n:0-9
    fn pri(n: i64) -> String {
        if n < 0 || 9 < n {
            panic!();
        }

        match n {
            0 => "〇",
            1 => "一",
            2 => "二",
            3 => "三",
            4 => "四",
            5 => "五",
            6 => "六",
            7 => "七",
            8 => "八",
            9 => "九",
            _ => unreachable!(),
        }
        .to_string()
    }

    fn low_term(res: &mut String, n: i64, pp_c: char, pp: i64) -> i64 {
        if n >= pp {
            if n / pp != 1 {
                res.push_str(&pri(n / pp));
            }

            res.push(pp_c);
            n - n / pp * pp
        } else {
            n
        }
    }

    //n:0-9999
    fn low_num(mut n: i64) -> String {
        if n < 0 || 9999 < n {
            panic!();
        }

        if n == 0 {
            pri(n)
        } else {
            let mut res = String::new();
            n = low_term(&mut res, n, '千', 1000);
            n = low_term(&mut res, n, '百', 100);
            n = low_term(&mut res, n, '十', 10);

            if n != 0 {
                res.push_str(&pri(n));
            }

            res
        }
    }

    fn term(res: &mut String, n: i64, pp_c: char, pp: i64) -> i64 {
        if n >= pp {
            res.push_str(&low_num(n / pp));
            res.push(pp_c);
            n - n / pp * pp
        } else {
            n
        }
    }

    pub fn num(mut n: i64) -> String {
        if n == 0 {
            pri(n)
        } else {
            let mut res = String::new();
            n = term(&mut res, n, '億', 100000000);
            n = term(&mut res, n, '万', 10000);

            if n != 0 {
                res.push_str(&low_num(n));
            }

            res
        }
    }

    #[test]
    fn test() {
        assert_eq!(num(10), "十".to_string());
    }
}

tests! {
    test1: "四の三の二乗乗\n" => "二十六万二千百四十四\n",
    test2: "四の三乗の二乗\n" => "四千九十六\n",
    test3: "十億十\n" => "一\n",
    test4: "一億二千三百四十五万六千七百八十九の二の〇の〇乗乗乗\n" => "六億千三百一万六千三百十九\n",
}
