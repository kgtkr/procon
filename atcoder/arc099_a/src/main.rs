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
    input!(input=>(n:i64 k:i64)(list:[i64]));
    let &min = list.iter().min().unwrap();
    let find = list
        .into_iter()
        .enumerate()
        .filter(|&(i, x)| x == min)
        .map(|(i, _)| i as i64)
        .collect::<Vec<_>>();

    let mut count = div(find[0] + 1, k);
    let mut cer = count * (k - 1);
    for x in find.into_iter().skip(1) {
        if cer + 1 == x {
            cer = x;
        } else if cer >= x {

        } else {
            let a = div(x - cer + 1, k);
            count += a;
            cer += a * (k - 1);
        }
    }
    //last
    count += div(n - cer - 1, k - 1);
    count.to_string()
}

fn div(a: i64, b: i64) -> i64 {
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
    test1: "4 3\n2 3 1 4" => "2",
    test2: "3 3\n1 2 3" => "1",
    test3: "8 3\n7 3 1 8 4 6 2 5" => "4",
    add1: "8 3\n2 1 2 2 2 1" => "4",
}

pub fn diff_seq(v: Vec<usize>) -> Vec<usize> {
    v.clone()
        .into_iter()
        .skip(1)
        .zip(v.into_iter())
        .map(|(a, b)| a - b)
        .collect()
}
