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
    input!(input=>(sx:i64 sy:i64 tx:i64 ty:i64));
    let w = (tx - sx) as usize;
    let h = (ty - sy) as usize;
    //(下)-[左]-[上]-(右)-(左)-[下]-[右]-(上)
    //座標の差をw/hとする
    //U*h、R*w、D*h、L*(w+1)、U*(h+1)、R*(w+1)、D*1、R*1、D*(h+1)、L*(w+1)、U*1
    //右:R、左:L、上:D、下:U
    std::iter::empty()
        .chain(std::iter::repeat('U').take(h))
        .chain(std::iter::repeat('R').take(w))
        .chain(std::iter::repeat('D').take(h))
        .chain(std::iter::repeat('L').take(w + 1))
        .chain(std::iter::repeat('U').take(h + 1))
        .chain(std::iter::repeat('R').take(w + 1))
        .chain(std::iter::repeat('D').take(1))
        .chain(std::iter::repeat('R').take(1))
        .chain(std::iter::repeat('D').take(h + 1))
        .chain(std::iter::repeat('L').take(w + 1))
        .chain(std::iter::repeat('U').take(1))
        .collect::<String>()
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
    test1: "0 0 1 2" => "UURDDLLUUURRDRDDDLLU",
    test2: "-2 -2 1 1" => "UURRURRDDDLLDLLULUUURRURRDDDLLDL",
}
