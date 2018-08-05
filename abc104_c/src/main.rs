extern crate core;

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

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = solve(input.trim().to_string());
    println!("{}", output);
}

fn solve(input: String) -> String {
    input!(input=>(d:usize g:i64){d;list:(i64,i64)});
    let mut v = list
        .into_iter()
        .enumerate()
        .map(|(a, (b, c))| ((a as i64) + 1, b, c))
        .collect::<Vec<_>>();
    v.reverse();
    f(g, &v, 0, 0, 0).to_string()
}

fn f(g: i64, list: &Vec<(i64, i64, i64)>, sum: i64, i: usize, count: i64) -> i64 {
    if sum >= g {
        count
    } else if i == list.len() {
        <i64>::max_value()
    } else {
        std::cmp::min(f(g, list, sum, i + 1, count), {
            let (a, b, c) = list[i];
            if a * b * 100 + sum >= g {
                let x = div2(g - sum, a * 100);
                f(g, list, sum + x * a * 100, i + 1, count + x)
            } else {
                f(g, list, sum + a * b * 100 + c, i + 1, count + b)
            }
        })
    }
}

fn div2(a: i64, b: i64) -> i64 {
    (a + (b - 1)) / b
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
    test1: "2 700\n3 500\n5 800" => "3",
    test2: "2 2000\n3 500\n5 800" => "7",
    test3: "2 400\n3 500\n5 800" => "2",
    test4: "5 25000\n20 1000\n40 1000\n50 1000\n30 1000\n1 1000" => "66",
}
