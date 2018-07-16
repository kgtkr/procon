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
    input!(input=>(n:usize k:i64)(list:[i64]));
    //必ず小さくなる
    //n log nまで
    //nとkがあればn-kを作れる
    //n-kとnがあればkを作れる
    //偶数-偶数=偶数
    //奇数-奇数=奇数
    //偶数-奇数=奇数
    //nとkn→n(k-1)
    //dpは無理

    /*
    全部作れる
    9 3 4
    6 5 1 2 7 8
     */

    /*
    倍数？
    6 9 3
     */

    //例4:偶数のみから奇数x

    /*10,1
    9 8 7 6 5 4 3 2
     */

    /*10 2
    8 6 5 4 2
     */

    /*
    10 3
    7 4 1
    ...
     */

    /*
    9 3
    6
     */

    /*
    11 3
    8 5 2
    9 1...
     */

    //1があればmax以下のを全部作れる

    //11 3 7 15
    //4 1...

    //公約数？

    let mut g = list[0];
    for x in list.clone() {
        g = gcd(g, x);
    }

    let max = list.into_iter().max().unwrap();

    if k <= max && k % g == 0 {
        "POSSIBLE"
    } else {
        "IMPOSSIBLE"
    }.to_string()
}

fn gcd(a: i64, b: i64) -> i64 {
    if b == 0 {
        a
    } else {
        gcd(b, a % b)
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
    test1: "3 7\n9 3 4" => "POSSIBLE",
    test2: "3 5\n6 9 3" => "IMPOSSIBLE",
    test3: "4 11\n11 3 7 15" => "POSSIBLE",
    test4: "5 12\n10 2 8 6 4" => "IMPOSSIBLE",
}
