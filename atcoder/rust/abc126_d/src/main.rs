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
  input!(input=>(n:usize){n-1;list:(@,@,i64)});
  let mut map = Vec::new();
  map.resize(n, Vec::new());
  let mut res = Vec::new();
  res.resize(n, false);
  for (a, b, c) in list {
    map[a].push((b, c % 2 == 0));
    map[b].push((a, c % 2 == 0));
  }
  f(&mut res, n + 10, &map, 0, false);
  res
    .into_iter()
    .map(|x| (if x { 1 } else { 0 }).to_string())
    .collect::<Vec<_>>()
    .join("\n")
    .to_string()
}

fn f(res: &mut Vec<bool>, old: usize, map: &Vec<Vec<(usize, bool)>>, i: usize, c: bool) {
  for &(a, w) in &map[i] {
    if a != old {
      let color = if w { c } else { !c };
      res[a] = color;
      f(res, i, map, a, color);
    }
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
    test1: "3\n1 2 2\n2 3 1\n" => "0\n0\n1\n",
    test2: "5\n2 5 2\n2 3 10\n1 3 8\n3 4 2\n" => "1\n0\n1\n0\n1\n",
}
