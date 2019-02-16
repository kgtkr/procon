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

fn to_n(x: usize) -> usize {
  match x {
    1 => 2,
    2 => 5, //
    3 => 5, //
    4 => 4,
    5 => 5,
    6 => 6, //
    7 => 3,
    8 => 7,
    9 => 6,
    _ => panic!(),
  }
}

fn solve(input: String) -> String {
  input!(input=>(n:usize m:usize)(list:[usize]));
  let mut list = list;
  list.sort();
  list.reverse();

  let mut dp = Vec::with_capacity(n + 1);
  dp.resize(n + 1, None);
  f(n, &list, &mut dp)
    .map(|x| {
      if x.len() == 0 {
        "0".to_string()
      } else {
        x.into_iter()
          .map(|x| x.to_string())
          .collect::<Vec<_>>()
          .join("")
      }
    })
    .unwrap_or("0".to_string())
}

fn vec_max(a: Vec<usize>, b: Vec<usize>) -> Vec<usize> {
  if a.len() > b.len() {
    a
  } else if a.len() < b.len() {
    b
  } else {
    for i in 0..a.len() {
      if a[i] > b[i] {
        return a;
      } else if a[i] < b[i] {
        return b;
      }
    }

    a
  }
}

//残り本数
fn f(n: usize, list: &Vec<usize>, dp: &mut Vec<Option<Option<Vec<usize>>>>) -> Option<Vec<usize>> {
  if let &Some(ref x) = &dp[n] {
    return x.clone();
  }

  let res = if n == 0 {
    Some(Vec::new())
  } else {
    let mut max = None;
    for &x in list {
      if n >= to_n(x) {
        max = match (
          max,
          f(n - to_n(x), list, dp).map(|mut a| {
            a.push(x);
            a
          }),
        ) {
          (Some(x), None) => Some(x),
          (None, Some(x)) => Some(x),
          (Some(a), Some(b)) => Some(vec_max(a, b)),
          (None, None) => None,
        };
      }
    }

    max
  };

  dp[n] = Some(res.clone());

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
    test1: "20 4\n3 7 8 4\n" => "777773\n",
    test2: "101 9\n9 8 7 6 5 4 3 2 1\n" => "71111111111111111111111111111111111111111111111111\n",
    test3: "15 3\n5 4 6\n" => "654\n",
}
