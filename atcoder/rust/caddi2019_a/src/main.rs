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
  input!(input=>(l:i64 n:usize m:usize){n;rp:(i64,i64)}{m;abcd:(#,#,i64,i64)});
  let mut rp = rp.into_iter().enumerate().collect::<Vec<_>>();
  rp.sort_by_key(|&(_, (x, _))| x);
  rp.reverse();

  let mut res = Vec::with_capacity(n);
  res.resize(n, (-1, -1, -1));

  make_space(l, &mut rp, &mut res);

  res
    .into_iter()
    .map((|(x, y, z)| format!("{} {} {}", x, y, z)))
    .collect::<Vec<_>>()
    .join("\n")
}

fn make_space(l: i64, rp: &mut Vec<(usize, (i64, i64))>, res: &mut Vec<(i64, i64, i64)>) {
  let mut cur_x = 0;
  loop {
    if let Some(&(_, (r, _))) = rp.last() {
      cur_x = make_side(l, cur_x, rp, res);
      if cur_x + r * 2 > l {
        break;
      }
    } else {
      break;
    }
  }
}

fn make_side(
  l: i64,
  cur_x: i64,
  rp: &mut Vec<(usize, (i64, i64))>,
  res: &mut Vec<(i64, i64, i64)>,
) -> i64 {
  let mut max_x = cur_x;
  let mut cur_y = 0;
  loop {
    if let Some(&(_, (r, _))) = rp.last() {
      let (x, y) = make_line(l, cur_x, cur_y, rp, res);
      cur_y = y;
      max_x = std::cmp::max(max_x, x);
      if cur_y + r * 2 > l {
        break;
      }
    } else {
      break;
    }
  }
  max_x
}

fn make_line(
  l: i64,
  cur_x: i64,
  cur_y: i64,
  rp: &mut Vec<(usize, (i64, i64))>,
  res: &mut Vec<(i64, i64, i64)>,
) -> (i64, i64) {
  let mut max_x = cur_x;
  let mut max_y = cur_y;
  let mut cur_z = 0;
  loop {
    if let Some(&(i, (r, _))) = rp.last() {
      if cur_z + r * 2 <= l && cur_y + r * 2 <= l && cur_x + r * 2 <= l {
        rp.pop();
        res[i] = (cur_x + r, cur_y + r, cur_z + r);
        max_x = std::cmp::max(max_x, cur_x + r * 2);
        max_y = std::cmp::max(max_y, cur_y + r * 2);
        cur_z += r * 2;
      } else {
        break;
      }
    } else {
      break;
    }
  }

  (max_x, max_y)
}
