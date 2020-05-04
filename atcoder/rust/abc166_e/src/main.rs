extern crate core;

use std::collections::HashMap;
use std::io::{self, Read};

#[macro_export]
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
  //インデックス(-1)
  ($line:expr,@) => {
    $line.next().unwrap().parse::<usize>().unwrap()-1
  };
  //単一値
  ($line:expr,$t:ty) => {
    $line.next().unwrap().parse::<$t>().unwrap()
  };
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = solve(input.trim().to_string());
    println!("{}", output);
}

fn solve(input: String) -> String {
    input!(input=>(n:usize)(list:[i64]));
    /*
    2 3 3 1 3 1

    1 2 (2, -1) (3, 0) (4, 1)
    2 3 (3, -2) (4, -1) (5, 0)
    3 3
    4 1
    5 3
    6 1

    (n_x+a_x)=(n_y-h_y)となるx<yな値の個数
    */
    let list = list
        .into_iter()
        .enumerate()
        .map(|(i, x)| (i as i64 + 1, x))
        .collect::<Vec<_>>();

    let diff_to_n = list.clone().into_iter().map(|(n, a)| (n - a, n)).fold(
        HashMap::new(),
        |mut acc, (key, v)| {
            acc.entry(key).or_insert(Vec::new()).push(v);
            acc
        },
    );

    list.into_iter()
        .map(|(n, a)| {
            diff_to_n
                .get(&(n + a))
                .map(|xs| xs.iter().filter(|n2| **n2 > n).count())
                .unwrap_or(0)
        })
        .sum::<usize>()
        .to_string()
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
    test1: "6\n2 3 3 1 3 1\n" => "3\n",
    test2: "6\n5 2 4 2 8 8\n" => "0\n",
    test3: "32\n3 1 4 1 5 9 2 6 5 3 5 8 9 7 9 3 2 3 8 4 6 2 6 4 3 3 8 3 2 7 9 5\n" => "22\n",
}
