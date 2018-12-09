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
    input!(input=>(n:usize)(list:[i64]));
    f(None, list.clone().into_iter().sum::<i64>(), list).to_string()
}

fn f(last: Option<usize>, sum: i64, list: Vec<i64>) -> i64 {
    if sum == 0 {
        1
    } else {
        let mut count = 0;
        for i in 0..list.len() {
            if list[i] != 0
                && Some(i) != last
                && Some(i + 1) != last
                && Some(i) != last.clone().map(|x| x + 1)
            {
                let mut list = list.clone();
                list[i] -= 1;
                count = add(count, (list[i] + 1) * f(Some(i), sum - 1, list));
            }
        }
        count
    }
}

const MOD: i64 = 10007;

pub fn power(x: i64, y: i64) -> i64 {
    if y == 0 {
        1
    } else if y == 1 {
        x % MOD
    } else if y % 2 == 0 {
        power(x, y / 2).pow(2) % MOD
    } else {
        power(x, y / 2).pow(2) * x % MOD
    }
}

pub fn div(a: i64, b: i64) -> i64 {
    mul(a, power(b, MOD - 2))
}

pub fn add(a: i64, b: i64) -> i64 {
    (a + b) % MOD
}

pub fn mul(a: i64, b: i64) -> i64 {
    ((a % MOD) * (b % MOD)) % MOD
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
    test1: "4
2 1 1 1" => "4",
    test2: "5
1 2 3 2 1" => "0",
    test3: "6
1 2 3 3 2 1" => "4754",
}
