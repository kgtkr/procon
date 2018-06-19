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
    let sum = seq::sum_seq(list);
    let mut map: HashMap<i64, i64> = HashMap::new();
    map_add(&mut map, 0, 1);
    for i in sum {
        map_add(&mut map, i, 1);
    }
    map.into_iter()
        .map(|(_, n)| n * (n - 1) / 2)
        .sum::<i64>()
        .to_string()
}

fn map_add(map: &mut HashMap<i64, i64>, key: i64, add: i64) {
    let v = match map.get(&key) {
        Some(v) => *v + add,
        None => add,
    };
    map.insert(key, v);
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
    test1: "6\n1 3 -4 2 2 -2" => "3",
    test2: "7\n1 -1 1 -1 1 -1 1" => "12",
    test3: "5\n1 -2 3 -4 5" => "0",
}

mod seq {
    pub fn diff_seq(v: Vec<i64>) -> Vec<i64> {
        v.clone()
            .into_iter()
            .skip(1)
            .zip(v.into_iter())
            .map(|(a, b)| a - b)
            .collect()
    }

    pub fn one_base_to_zero_base(v: Vec<usize>) -> Vec<usize> {
        v.into_iter().map(|x| x - 1).collect()
    }

    pub fn sum_seq(v: Vec<i64>) -> Vec<i64> {
        v.into_iter()
            .scan(0, |state, x| {
                *state = *state + x;
                Some(*state)
            })
            .collect()
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn diff_seq_test() {
            assert_eq!(vec![-9, 1, 0, 1], diff_seq(vec![10, 1, 2, 2, 3]));
            assert_eq!(vec![] as Vec<i64>, diff_seq(vec![10]));
        }

        #[test]
        fn one_base_to_zero_base_test() {
            assert_eq!(
                vec![0, 1, 3, 1, 2],
                one_base_to_zero_base(vec![1, 2, 4, 2, 3])
            );
            assert_eq!(vec![] as Vec<usize>, one_base_to_zero_base(vec![]));
        }

        #[test]
        fn sum_seq_test() {
            assert_eq!(vec![1, 1, 3, 7, 9, 12], sum_seq(vec![1, 0, 2, 4, 2, 3]));
            assert_eq!(vec![] as Vec<i64>, sum_seq(vec![]));
        }
    }
}
