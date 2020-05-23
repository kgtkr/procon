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

#[derive(Copy, Clone, Debug)]
enum Pert {
    Bot(usize),
    Chi(usize),
}

fn solve(input: String) -> String {
    input!(input=>(n:usize q:usize){q;qs:(usize,usize,usize)});
    let mut state = (0..n).map(|b| Pert::Bot(b)).collect::<Vec<_>>();
    let mut tops = (0..n).map(Some).collect::<Vec<_>>();

    for (a, b, i) in qs {
        let a = a - 1;
        let b = b - 1;
        let i = i - 1;
        let prev_i_pert = state[i];
        state[i] = tops[b].map(Pert::Chi).unwrap_or(Pert::Bot(b));
        tops[b] = Some(tops[a].unwrap());
        tops[a] = match prev_i_pert {
            Pert::Bot(_) => None,
            Pert::Chi(x) => Some(x),
        };
    }

    let mut uf = UnionFind::new(n);

    for (i, state) in state.iter().enumerate() {
        if let Pert::Chi(x) = state {
            uf.unite(i, *x);
        }
    }

    // 親がbaseのnode idの根→親
    let mut map = HashMap::new();
    for (i, state) in state.iter().enumerate() {
        if let Pert::Bot(x) = state {
            map.insert(uf.find(i), x);
        }
    }

    (0..n)
        .map(|x| (**map.get(&uf.find(x)).unwrap() + 1).to_string())
        .collect::<Vec<_>>()
        .join("\n")
}

#[derive(PartialEq, Debug, Clone)]
pub struct UnionFindNode {
    pub par: usize,
    pub rank: usize,
}

#[derive(PartialEq, Debug, Clone)]
pub struct UnionFind(pub Vec<UnionFindNode>);

impl UnionFind {
    //初期化
    pub fn new(size: usize) -> UnionFind {
        let mut vec = Vec::new();
        for i in 0..size {
            vec.push(UnionFindNode { par: i, rank: 0 });
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
    pub fn unite(&mut self, x: usize, y: usize) {
        let x = self.find(x);
        let y = self.find(y);
        if x == y {
            return;
        }

        if self.0[x].rank < self.0[y].rank {
            self.0[x].par = y;
        } else {
            self.0[y].par = x;
            if self.0[x].rank == self.0[y].rank {
                self.0[x].rank += 1;
            }
        }
    }

    //xとyが同じ集合に属するか
    pub fn same(&mut self, x: usize, y: usize) -> bool {
        self.find(x) == self.find(y)
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
    test1: "3 4\n1 2 1\n2 3 2\n3 1 3\n1 3 2\n" => "3\n3\n1\n",
    test2: "10 20\n3 6 3\n6 5 6\n10 8 10\n5 7 3\n1 3 1\n4 10 4\n5 4 6\n10 7 4\n7 9 3\n9 8 4\n8 1 4\n3 7 1\n2 3 2\n9 8 3\n8 1 10\n8 2 8\n9 10 9\n2 1 8\n4 9 6\n1 7 4\n" => "7\n3\n7\n7\n5\n9\n7\n7\n10\n7\n",
}
