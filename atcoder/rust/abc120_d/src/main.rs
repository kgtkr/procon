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
  input!(input=>(n:usize m:usize){m;list:(@,@)});
  let mut list = list;
  list.reverse();
  let mut res = Vec::new();
  let mut uf = UnionFind::new(n);
  for (a, b) in list {
    res.push(uf.unite(a, b));
  }
  res.reverse();
  sum_seq(res)
    .into_iter()
    .map(|x| x.to_string())
    .collect::<Vec<_>>()
    .join("\n")
}

#[derive(PartialEq, Debug, Clone)]
pub struct UnionFindNode {
  pub par: usize,
  pub rank: usize,
  pub count: usize,
}

#[derive(PartialEq, Debug, Clone)]
pub struct UnionFind(pub Vec<UnionFindNode>);

impl UnionFind {
  //初期化
  pub fn new(size: usize) -> UnionFind {
    let mut vec = Vec::new();
    for i in 0..size {
      vec.push(UnionFindNode {
        par: i,
        rank: 0,
        count: 1,
      });
    }

    UnionFind(vec)
  }

  //根を求める
  pub fn find(&mut self, x: usize) -> usize {
    if self.0[x].par == x {
      x
    } else {
      let par = self.0[x].par;
      let v = self.find(par);
      self.0[x].par = v;
      v
    }
  }

  //xとyの集合を併合
  pub fn unite(&mut self, x: usize, y: usize) -> usize {
    let x = self.find(x);
    let y = self.find(y);
    if x == y {
      return 0;
    }

    let res = self.0[x].count * self.0[y].count;
    if self.0[x].rank < self.0[y].rank {
      self.0[x].par = y;
      self.0[y].count += self.0[x].count;
    } else {
      self.0[y].par = x;
      self.0[x].count += self.0[y].count;
      if self.0[x].rank == self.0[y].rank {
        self.0[x].rank += 1;
      }
    }

    res
  }

  //xとyが同じ集合に属するか
  pub fn same(&mut self, x: usize, y: usize) -> bool {
    self.find(x) == self.find(y)
  }
}

pub fn sum_seq(v: Vec<usize>) -> Vec<usize> {
  v.into_iter()
    .scan(0, |state, x| {
      *state = *state + x;
      Some(*state)
    })
    .collect()
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
    test1: "4 5\n1 2\n3 4\n1 3\n2 3\n1 4\n" => "0\n0\n4\n5\n6\n",
    test2: "6 5\n2 3\n1 2\n5 6\n3 4\n4 5\n" => "8\n9\n12\n14\n15\n",
    test3: "2 1\n1 2\n" => "1\n",
}
