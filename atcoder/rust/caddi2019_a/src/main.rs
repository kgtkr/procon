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
  rp.sort_by_key(|&(_, (r, p))| (r, p));
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
  let mut cur_y = RMQRUQ::new(l as usize);
  loop {
    if let Some(&(_, (r, _))) = rp.last() {
      let x = make_line(l, cur_x, &mut cur_y, rp, res);
      max_x = std::cmp::max(max_x, x);
      if cur_y.query_f(0, l as usize) + r * 2 > l {
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
  cur_y: &mut RMQRUQ,
  rp: &mut Vec<(usize, (i64, i64))>,
  res: &mut Vec<(i64, i64, i64)>,
) -> i64 {
  let mut max_x = cur_x;
  let mut cur_z = 0;
  loop {
    if let Some(&(i, (r, _))) = rp.last() {
      let query = cur_y.query_f(cur_z as usize, (cur_z + r * 2) as usize);
      if cur_z + r * 2 <= l && query + r * 2 <= l && cur_x + r * 2 <= l {
        rp.pop();
        res[i] = (cur_x + r, query + r, cur_z + r);
        max_x = std::cmp::max(max_x, cur_x + r * 2);
        cur_y.update_f(cur_z as usize, (cur_z + r * 2) as usize, query + r * 2);
        cur_z += r * 2;
      } else {
        break;
      }
    } else {
      break;
    }
  }

  max_x
}

//直径2
//0,1

struct RMQRUQ {
  n: usize,
  dat: Vec<i64>,
  lazy: Vec<i64>,
}

impl RMQRUQ {
  fn new(n_: usize) -> RMQRUQ {
    let mut n = 1;
    while n < n_ {
      n *= 2;
    }
    let mut dat = Vec::with_capacity(n * 2);
    dat.resize(n * 2, 0);
    let mut lazy = Vec::with_capacity(n * 2);
    lazy.resize(n * 2, 0);
    RMQRUQ {
      n: n,
      dat: dat,
      lazy: lazy,
    }
  }

  fn eval(&mut self, len: usize, k: usize) {
    if self.lazy[k] == 0 {
      return;
    }
    if k * 2 + 1 < self.n * 2 - 1 {
      self.lazy[2 * k + 1] = self.lazy[k];
      self.lazy[2 * k + 2] = self.lazy[k];
    }
    self.dat[k] = self.lazy[k];
    self.lazy[k] = 0;
  }

  // [a, b)
  fn update(&mut self, a: usize, b: usize, x: i64, k: usize, l: usize, r: usize) -> i64 {
    self.eval(r - l, k);
    if b <= l || r <= a {
      return self.dat[k];
    }
    if a <= l && r <= b {
      self.lazy[k] = x;
      return self.lazy[k];
    }
    self.dat[k] = std::cmp::max(
      self.update(a, b, x, 2 * k + 1, l, (l + r) / 2),
      self.update(a, b, x, 2 * k + 2, (l + r) / 2, r),
    );
    self.dat[k]
  }
  fn update_f(&mut self, a: usize, b: usize, x: i64) -> i64 {
    let n = self.n;
    return self.update(a, b, x, 0, 0, n);
  }

  // [a, b)
  fn query(&mut self, a: usize, b: usize, k: usize, l: usize, r: usize) -> i64 {
    self.eval(r - l, k);
    if b <= l || r <= a {
      return 0;
    }
    if a <= l && r <= b {
      return self.dat[k];
    }
    let vl = self.query(a, b, 2 * k + 1, l, (l + r) / 2);
    let vr = self.query(a, b, 2 * k + 2, (l + r) / 2, r);
    return std::cmp::max(vl, vr);
  }
  fn query_f(&mut self, a: usize, b: usize) -> i64 {
    let n = self.n;
    return self.query(a, b, 0, 0, n);
  }
}
