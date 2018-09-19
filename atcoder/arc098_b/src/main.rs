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
    input!(input=>(n:usize)(list:[i64]));
    let sum = list
        .clone()
        .into_iter()
        .scan(0, |state, x| {
            *state = *state + x;
            Some(*state)
        })
        .collect::<Vec<_>>();

    let xor = list
        .clone()
        .into_iter()
        .scan(0, |state, x| {
            *state = *state ^ x;
            Some(*state)
        })
        .enumerate()
        .collect::<Vec<_>>();

    let mut map = HashMap::new();
    for (i, x) in xor {
        let ent = map.entry(x).or_insert(vec![]);
        ent.push(i);
    }

    println!("xor_map:{:?}", map);

    let mut count = 0;
    for (i, x) in sum.into_iter().enumerate() {
        match map.get(&x) {
            Some(bts) => {
                let (a, b, c) = bound::bound_count(&bts, &i);
                count += a + b;
            }
            _ => {}
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
    test1: "4\n2 5 4 6" => "5",
    test2: "9\n0 0 0 0 0 0 0 0 0" => "45",
    test3: "19\n885 8 1 128 83 32 256 206 639 16 4 128 689 32 8 64 885 969 1" => "37",
}
