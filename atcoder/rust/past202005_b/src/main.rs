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
  input!(input=>(n:usize m:usize q:usize){q;list:[usize]});
  // 問題iを解いた参加者
  let mut ac = Vec::with_capacity(m);
  ac.resize(m, Vec::<usize>::new());

  // 各参加者iの点数
  let mut points = Vec::with_capacity(n);
  points.resize(n, 0);

  let mut res = Vec::new();

  for q in list {
    match &q[..] {
      &[1, a] => {
        let a = a - 1;
        res.push(points[a]);
      }
      &[2, a, b] => {
        let a = a - 1;
        let b = b - 1;
        points[a] += n - ac[b].len();
        ac[b].push(a);
        for &i in &ac[b] {
          points[i] -= 1;
        }
      }
      s => panic!("{:?}", s),
    }
  }

  res
    .into_iter()
    .map(|x| x.to_string())
    .collect::<Vec<_>>()
    .join("\n")
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
  test1: "2 1 6\n2 1 1\n1 1\n1 2\n2 2 1\n1 1\n1 2\n" => "1\n0\n0\n0\n",
  test2: "5 5 30\n1 3\n2 3 5\n1 3\n2 2 1\n2 4 5\n2 5 2\n2 2 3\n1 4\n2 4 1\n2 2 2\n1 1\n1 5\n2 5 3\n2 4 4\n1 4\n1 2\n2 3 3\n2 4 3\n1 3\n1 5\n1 3\n2 1 3\n1 1\n2 2 4\n1 1\n1 4\n1 5\n1 4\n1 1\n1 5\n" => "0\n4\n3\n0\n3\n10\n9\n4\n4\n4\n0\n0\n9\n3\n9\n0\n3\n",
}
