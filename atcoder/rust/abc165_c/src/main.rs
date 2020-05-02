extern crate core;

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
    input!(input=>(n:usize m:i64 q:usize){q;list:(usize,usize,i64,i64)});
    f(Vec::new(), 1, n, m, &list).to_string()
}

fn f(a_list: Vec<i64>, min: i64, n: usize, m: i64, list: &Vec<(usize, usize, i64, i64)>) -> i64 {
    if a_list.len() == n {
        list.iter()
            .filter(|(a, b, c, _)| a_list[b - 1] - a_list[a - 1] == *c)
            .map(|(_, _, _, d)| d)
            .sum::<i64>()
    } else {
        (min..=m)
            .map(|min| {
                let mut a_list = a_list.clone();
                a_list.push(min);
                f(a_list, min, n, m, list)
            })
            .max()
            .unwrap()
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
    test1: "3 4 3\n1 3 3 100\n1 2 2 10\n2 3 2 10\n" => "110\n",
    test2: "4 6 10\n2 4 1 86568\n1 4 0 90629\n2 3 0 90310\n3 4 1 29211\n3 4 3 78537\n3 4 2 8580\n1 2 1 96263\n1 4 2 2156\n1 2 0 94325\n1 4 3 94328\n" => "357500\n",
    test3: "10 10 1\n1 10 9 1\n" => "1\n",
}
