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
    input!(input=>(n:i64));
    ((power(4, n) - (23 * power(8, n - 3) - 2) / 7) % MOD).to_string()
}

/*
f(3)=3
f(n)=8*f(n-1)+2

f(n)=(23*8^(n-3)-2)/7

4^n-f(n)
*/

const MOD: i64 = 1000000007;

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
    test1: "3\n" => "61\n",
    test2: "4\n" => "230\n",
    test3: "100\n" => "388130742\n",
}
