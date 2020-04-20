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

/*
0-5から2つ選ぶ時
最も大きい2つ-最も小さい2つ+1
n+(n-1) - 0+1 + 1

9-1+1

0 1
0 2
0 3
0 4
0 5

1 5
2 5
3 5
4 5

0-5から3つ選ぶ時
最も小さい3つから最も大きい3つ

0 1 2 - 3
0 1 3 - 4
0 1 4 - 5
0 1 5 - 6

1 2 5 - 7
1 3 5 - 8
1 4 5 - 9

2 3 5 - 10
*/

fn solve(input: String) -> String {
  input!(input=>(n:i64 k:i64));
  let mut result = 0;
  for x in k..=n + 1 {
    result = add(result, (x * n - x * (x - 1) / 2) - x * (x - 1) / 2 + 1);
  }
  result.to_string()
}
const MOD: i64 = 1000000007;

pub fn add(a: i64, b: i64) -> i64 {
  (a + b) % MOD
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
  test1: "3 2\n" => "10\n",
  test2: "200000 200001\n" => "1\n",
  test3: "141421 35623\n" => "220280457\n",
}
