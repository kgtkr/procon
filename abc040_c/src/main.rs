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

fn diff_seq(v: Vec<i64>) -> Vec<i64> {
    v.clone()
        .into_iter()
        .skip(1)
        .zip(v.into_iter())
        .map(|(a, b)| a - b)
        .collect()
}

//i番目以降の最小
fn f(dp: &mut Vec<i64>, list: &Vec<i64>, i: usize) -> i64 {
    let n = list.len();

    if dp[i] != -1 {
        return dp[i].clone();
    }

    let res = if i == n - 1 {
        0
    } else if i == n - 2 {
        f(dp, list, i + 1) + (list[i] - list[i + 1]).abs()
    } else {
        let a = (list[i] - list[i + 1]).abs();
        let b = (list[i] - list[i + 2]).abs();
        std::cmp::min(
            f(dp, list, i + 1) + (list[i] - list[i + 1]).abs(),
            f(dp, list, i + 2) + (list[i] - list[i + 2]).abs(),
        )
    };

    dp[i] = res;
    res
}

fn solve(input: String) -> String {
    input!(input=>(n:usize)(list:[i64]));
    let mut dp = Vec::with_capacity(n);
    dp.resize(n, -1);
    f(&mut dp, &list, 0).to_string()
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
    test1: "4\n100 150 130 120" => "40",
    test2: "4\n100 125 80 110" => "40",
    test3: "9\n314 159 265 358 979 323 846 264 338" => "310",
}
