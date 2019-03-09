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
  input!(input=>(a:i64 b:i64));
  //41桁くらい
  let mut arr = Vec::new();
  arr.resize(41, 0);
  for i in 0..arr.len() {
    arr[i] = xn(b, (i + 1) as u32) - xn(a - 1, (i + 1) as u32);
  }

  let mut p = 1;
  let mut res = 0;
  for &x in &arr {
    res += p * (x % 2);
    p *= 2;
  }
  res.to_string()
}

fn xn(x: i64, n: u32) -> i64 {
  let p = 2i64.pow(n);
  (x + 1) / p * (p / 2) + std::cmp::max(0, (x + 1) % p - p / 2)
}

#[test]
fn xn_test() {
  assert_eq!(xn(1, 1), 1);
  assert_eq!(xn(0, 1), 0);
  assert_eq!(xn(6, 3), 3);
  assert_eq!(xn(4, 2), 2);
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
    test1: "2 4\n" => "5\n",
    test2: "123 456\n" => "435\n",
    test3: "123456789012 123456789012\n" => "123456789012\n",
}
