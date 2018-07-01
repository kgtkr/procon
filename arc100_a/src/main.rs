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
    input!(input=>(n:usize)(list:[i64]));
    let n = a + b;
    n.to_string()
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
    test1: "5\n2 2 3 5 5" => "2",
    test2: "9\n1 2 3 4 5 6 7 8 9" => "0",
    test3: "6\n6 5 4 3 2 1" => "18",
    test4: "7\n1 1 1 1 2 3 4" => "6",
}
