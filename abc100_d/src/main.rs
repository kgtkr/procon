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

fn f(list: Vec<(i64, i64, i64)>, m: usize, x: i64, y: i64, z: i64) -> i64 {
    let mut list = list
        .into_iter()
        .map(|(a, b, c)| a * x + b * y + c * z)
        .collect::<Vec<_>>();
    list.sort_by(|a, b| b.cmp(a));
    list.into_iter().take(m).fold(0, |sum, i| sum + i)
}

fn solve(input: String) -> String {
    input!(input=>(n:usize m:usize){n;list:(i64,i64,i64)});

    let mut v = vec![
        f(list.clone(), m, 1, 1, 1),
        f(list.clone(), m, -1, 1, 1),
        f(list.clone(), m, 1, -1, 1),
        f(list.clone(), m, 1, 1, -1),
        f(list.clone(), m, 1, -1, -1),
        f(list.clone(), m, -1, 1, -1),
        f(list.clone(), m, -1, -1, 1),
        f(list.clone(), m, -1, -1, -1),
    ];
    v.sort_by(|a, b| b.cmp(a));
    v[0].to_string()
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
    test1: "5 3\n3 1 4\n1 5 9\n2 6 5\n3 5 8\n9 7 9" => "56",
    test2: "5 3\n1 -2 3\n-4 5 -6\n7 -8 -9\n-10 11 -12\n13 -14 15" => "54",
    test3: "10 5\n10 -80 21\n23 8 38\n-94 28 11\n-26 -2 18\n-69 72 79\n-26 -86 -54\n-72 -50 59\n21 65 -32\n40 -94 87\n-62 18 82" => "638",
    test4: "3 2\n2000000000 -9000000000 4000000000\n7000000000 -5000000000 3000000000\n6000000000 -1000000000 8000000000" => "30000000000",
}
