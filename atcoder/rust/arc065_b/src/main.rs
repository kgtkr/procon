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
    input!(input=>(n:usize k:usize l:usize){k;k_list:(@,@)}{l;l_list:(@,@)});
    f(n, &k_list, &l_list)
        .into_iter()
        .zip(f(n, &l_list, &k_list))
        .map(|(a, b)| std::cmp::max(a, b).to_string())
        .collect::<Vec<_>>()
        .join(" ")
        .to_string()
}

fn f(n: usize, k_list: &Vec<(usize, usize)>, l_list: &Vec<(usize, usize)>) -> Vec<i32> {
    let mut k_uf = UnionFind::new(n);
    for &(a, b) in k_list {
        k_uf.unite(a, b);
    }
    let mut l_uf = UnionFind::new(n);
    for &(a, b) in l_list {
        if k_uf.same(a, b) {
            l_uf.unite(a, b);
        }
    }
    let mut count_vec = Vec::with_capacity(n);
    count_vec.resize(n, 0);
    for i in 0..n {
        count_vec[l_uf.find(i)] += 1;
    }
    let mut result = Vec::with_capacity(n);
    for i in 0..n {
        result.push(count_vec[l_uf.find(i)]);
    }
    result
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
    test1: "4 3 1\n1 2\n2 3\n3 4\n2 3" => "1 2 2 1",
    test2: "4 2 2\n1 2\n2 3\n1 4\n2 3" => "1 2 2 1",
    test3: "7 4 4\n1 2\n2 3\n2 5\n6 7\n3 5\n4 5\n3 4\n6 7" => "1 1 2 1 2 2 2",
}
