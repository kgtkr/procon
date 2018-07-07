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
    input!(input=>(t:usize){t;list:i64});
    let mut res = Vec::new();
    for x in list {
        println!("====={}=====", x);
        let mut min = (<i64>::max_value(), <i64>::max_value());
        //x=a+b、a>=bとなるaとbに分解
        for a in x / 2..x {
            let b = x - a;
            if a >= b {
                let v = f(a, b);
                println!("{:?} {:?}", (a, b), v);
                if min.0 > v.0 || (min.0 == v.0 && min.1 > v.1) {
                    min = v;
                }
            }
        }
        res.push(min);
    }
    //足してx
    //14→9/5→5/4→4/1→
    //14→
    res.into_iter()
        .map(|(a, b)| format!("{} {}", a, b).to_string())
        .collect::<Vec<_>>()
        .join("\n")
}

//a>b
fn f(a: i64, b: i64) -> (i64, i64) {
    //bとc(c=a-b)に分解
    let c = a - b;
    if b > 0 && c > 0 && b >= c {
        f(b, c)
    } else {
        (b, a)
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
    test1: "3\n14\n233\n100" => "1 4\n1 1\n1 99",
}
