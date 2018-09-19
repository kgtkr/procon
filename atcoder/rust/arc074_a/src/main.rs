extern crate core;
use std::cmp::{max, min};

use std::io::{self, Read};

#[macro_use]
mod parser {
    macro_rules! input {
    ($s:expr=>$($t:tt)*) => {
        let mut lines=$s.split("\n");
        $(
            line_parse!(lines,$t);
        )*
    };
    }

    macro_rules! line_parse {
    ($lines:expr,($($name:ident:$t:tt)*)) => {
        let mut line=$lines.next().unwrap().split_whitespace();
        $(value_def!(line,$name,$t);)*
    };

    //複数行
    ($lines:expr,{$n:expr;$name:ident:$t:tt}) => {
        values_def!($lines,$n,$name,$t);
    };
    }

    macro_rules! value_def {
        ($line:expr, $name:ident, $t:tt) => {
            let $name = value!($line, $t);
        };
    }

    macro_rules! values_def {
        ($lines:expr, $n:expr, $name:ident, $t:tt) => {
            let $name = {
                let mut vec = Vec::new();
                for i in 0..$n {
                    let mut next = $lines.next().unwrap().split_whitespace();
                    vec.push(value!(next, $t));
                }
                vec
            };
        };
    }

    macro_rules! value {
    //配列
    ($line:expr,[$t:tt]) => {
        $line.map(|x|{
        let mut iter=::std::iter::once(x);
        value!(iter,$t)
        }).collect::<Vec<_>>()
    };
    //タプル
    ($line:expr,($($t:tt),*)) => {
        ($(value!($line,$t),)*)
    };
    //文字列
    ($line:expr,#) => {
        $line.next().unwrap()
    };
    //単一値
    ($line:expr,$t:ty) => {
        $line.next().unwrap().parse::<$t>().unwrap()
    };
    }
}

macro_rules! min {
  ($x:expr) => {
    $x
  };
  ($x:expr, $($xs:tt)+) => {
    min($x,min!($($xs)+))
  };
}

macro_rules! max {
  ($x:expr) => {
    $x
  };
  ($x:expr, $($xs:tt)+) => {
    max($x,max!($($xs)+))
  };
}

fn three(w: i64, h: i64, a: i64, b: i64) -> i64 {
    let c = w * h - a - b;
    max!(a, b, c) - min!(a, b, c)
}

fn f(w: i64, h: i64, w2: i64, h2: i64) -> i64 {
    let a = w2 * h;
    let b = (w - w2) * h2;
    three(w, h, a, b)
}
fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = solve(input.trim().to_string());
    println!("{}", output);
}

fn solve(input: String) -> String {
    input!(input=>(h:i64 w:i64));
    min!(min1(w, h), min1(h, w), min2(w, h), min2(h, w)).to_string()
}

fn min1(w: i64, h: i64) -> i64 {
    if h == 2 {
        return <i64>::max_value();
    }

    if h % 3 == 0 {
        0
    } else {
        w
    }
}

fn min2(w: i64, h: i64) -> i64 {
    if w == 2 {
        f(w, h, 1, h / 2)
    } else if w % 3 == 0 {
        f(w, h, w / 3, h / 2)
    } else if w % 3 == 1 {
        min(f(w, h, w / 3, h / 2), f(w, h, w / 3 + 1, h / 2))
    } else {
        f(w, h, w / 3 + 1, h / 2)
    }
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

tests! {
    test1: "3 5" => "0",
    test2: "4 5" => "2",
    test3: "5 5" => "4",
    test4: "100000 2" => "1",
    test5: "100000 100000" => "50000",
}
