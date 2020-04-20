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
  let mut list = list.into_iter().enumerate().collect::<Vec<_>>();
  list.sort_by_key(|(_, x)| *x);
  list.reverse();

  let mut memo = {
    let mut v = Vec::with_capacity(n + 1);
    v.resize(n + 1, {
      let mut v = Vec::with_capacity(n + 1);
      v.resize(n + 1, None);
      v
    });
    v
  };
  (0..=n)
    .map(|l| {
      let r = n - l;
      f(n, &list, &mut memo, l, r)
    })
    .max()
    .unwrap()
    .to_string()
}

fn calc_happy((from, active): (usize, i64), to: usize) -> i64 {
  active * (from as i64 - to as i64).abs()
}

// 左にl人、右にr人移動するときの最大値
// リスト、次並べる場所、各幼児が利用済みか、嬉しさの合計
// i以降の席に並べるときの嬉しさの最大値
// 左に移動をl、右に移動をr
fn f(
  n: usize,
  list: &Vec<(usize, i64)>,
  memo: &mut Vec<Vec<Option<i64>>>,
  l: usize,
  r: usize,
) -> i64 {
  if let Some(res) = memo[l][r] {
    return res;
  }

  let res = match (l, r) {
    (0, 0) => 0,
    (1, 0) => calc_happy(list[0], 0),
    (0, 1) => calc_happy(list[0], n - 1),
    (l, r) => std::cmp::max(
      if l != 0 {
        f(n, list, memo, l - 1, r) + calc_happy(list[l + r - 1], l - 1)
      } else {
        0
      },
      if r != 0 {
        f(n, list, memo, l, r - 1) + calc_happy(list[l + r - 1], n - r)
      } else {
        0
      },
    ),
  };

  memo[l][r] = Some(res);

  res
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
  test1: "4\n1 3 4 2\n" => "20\n",
  test2: "6\n5 5 6 1 1 1\n" => "58\n",
  test3: "6\n8 6 9 1 2 1\n" => "85\n",
}
