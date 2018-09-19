extern crate core;

use std::collections::HashMap;
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
    input!(input=>(n:usize m:usize){m;list:(usize,usize)});
    let list = list
        .into_iter()
        .map(|(a, b)| (a - 1, b - 1))
        .collect::<Vec<_>>();
    let mut vec = Vec::with_capacity(n);
    vec.resize(n, None);
    for (a, b) in list.clone() {
        if vec[a].map(|x| x > b).unwrap_or(true) {
            vec[a] = Some(b);
        }
    }
    let mut count = 0;
    let mut min = None;
    for a in 0..n {
        let b = vec[a];
        if let Some(b) = b {
            if let Some(min_x) = min {
                if min_x <= a {
                    count += 1;
                    min = None;
                } else {
                    min = Some(std::cmp::min(min_x, b));
                }
            } else {
                count += 1;
                min = Some(b);
            }
        }
    }
    count.to_string()
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
    test1: "5 2\n1 4\n2 5" => "1",
    test2: "9 5\n1 8\n2 7\n3 5\n4 6\n7 9" => "2",
    test3: "5 10\n1 2\n1 3\n1 4\n1 5\n2 3\n2 4\n2 5\n3 4\n3 5\n4 5" => "4",
}
