extern crate core;

use std::collections::HashSet;
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
    input!(input=>(n:i64));
    let mut res = Vec::new();
    if n % 4 == 0 {
        let mut a = Vec::new();
        let mut b = Vec::new();
        for i in 0..n / 2 {
            if i % 2 == 0 {
                a.push(i * 2 + 1);
                b.push(i * 2 + 2);
            } else {
                b.push(i * 2 + 1);
                a.push(i * 2 + 2);
            }
        }
        for x in a {
            for &y in &b {
                res.push((x, y));
            }
        }
    } else if n % 2 == 0 {
        for i in 0..(n / 2 - 1) {
            let a = n - i;
            let b = i + 1;
            let c = n - (i + 1);
            let d = i + (1 + 1);
            res.push((a, c));
            res.push((a, d));
            res.push((b, c));
            res.push((b, d));
        }

        let i = n / 2 - 1;
        let a = n - i;
        let b = i + 1;
        let c = n - (0 + 1);
        let d = 0 + (1 + 1);
        res.push((n / 2, 1));
        res.push((n / 2, n));
        res.push((n / 2 + 1, 1));
        res.push((n / 2 + 1, n));
    } else {
        for i in 0..((n - 1) / 2 - 1) {
            let a = (n - 1) - i;
            let b = i + 1;
            let c = (n - 1) - (i + 1);
            let d = i + (1 + 1);
            res.push((a, c));
            res.push((a, d));
            res.push((b, c));
            res.push((b, d));
        }
        res.push((n, 1));
        res.push((n, n - 1));
        if n != 3 {
            res.push((n / 2, n));
            res.push((n / 2 + 1, n));
        }
    }
    let res = res
        .into_iter()
        .map(|(a, b)| (std::cmp::min(a, b), std::cmp::max(a, b)))
        .filter(|&(a, b)| a != b)
        .collect::<HashSet<_>>()
        .into_iter()
        .collect::<Vec<_>>();
    format!(
        "{}\n{}",
        res.len(),
        res.into_iter()
            .map(|(a, b)| format!("{} {}", a, b))
            .collect::<Vec<_>>()
            .join("\n")
    )
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
    test1: "3\n" => "2\n1 3\n2 3\n",
}
