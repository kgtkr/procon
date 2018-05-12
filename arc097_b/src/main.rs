extern crate core;

use std::io::{self, Read};

#[macro_use]
mod parser {
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
    ($line:expr,$name:ident,$t:tt) => {
        let $name=value!($line,$t);
    };
    }

    macro_rules! values_def {
    ($lines:expr,$n:expr,$name:ident,$t:tt) => {
        let $name={
        let mut vec=Vec::new();
        for i in 0..$n{
            let mut next=$lines.next().unwrap().split_whitespace();
            vec.push(value!(next,$t));
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
    //単一値
    ($line:expr,$t:ty) => {
        $line.next().unwrap().parse::<$t>().unwrap()
    };
    }
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = solve(input.trim().to_string());
    println!("{}", output);
}

fn solve(input: String) -> String {
    input!(input=>(n:usize m:usize)(p:[usize]){m;swap:(usize,usize)});
    let swap = swap.into_iter()
        .map(|(a, b)| (a - 1, b - 1))
        .collect::<Vec<_>>();
    let p = p.into_iter().map(|x| x - 1).collect::<Vec<_>>();

    let mut uf = uf::UnionFind::new(n);
    for (x, y) in swap {
        uf.unite(x, y);
    }

    let mut count = 0;

    for i in 0..n {
        if i == p[i] || uf.same(i, p[i]) {
            count += 1;
        }
    }

    count.to_string()
}

mod uf {
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

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn new() {
            assert_eq!(
                UnionFind(vec![
                    UnionFindNode { par: 0, rank: 0 },
                    UnionFindNode { par: 1, rank: 0 },
                    UnionFindNode { par: 2, rank: 0 },
                ]),
                UnionFind::new(3)
            );
        }

        #[test]
        fn find() {
            let mut uf = UnionFind(vec![
                UnionFindNode { par: 0, rank: 1 },
                UnionFindNode { par: 0, rank: 0 },
                UnionFindNode { par: 1, rank: 0 },
                UnionFindNode { par: 3, rank: 0 },
                UnionFindNode { par: 4, rank: 0 },
            ]);

            {
                let mut uf = uf.clone();
                assert_eq!(0, uf.find(0));
                assert_eq!(
                    UnionFind(vec![
                        UnionFindNode { par: 0, rank: 1 },
                        UnionFindNode { par: 0, rank: 0 },
                        UnionFindNode { par: 1, rank: 0 },
                        UnionFindNode { par: 3, rank: 0 },
                        UnionFindNode { par: 4, rank: 0 },
                    ]),
                    uf
                );
            }

            {
                let mut uf = uf.clone();
                assert_eq!(0, uf.find(1));
                assert_eq!(
                    UnionFind(vec![
                        UnionFindNode { par: 0, rank: 1 },
                        UnionFindNode { par: 0, rank: 0 },
                        UnionFindNode { par: 1, rank: 0 },
                        UnionFindNode { par: 3, rank: 0 },
                        UnionFindNode { par: 4, rank: 0 },
                    ]),
                    uf
                );
            }
            {
                let mut uf = uf.clone();
                for _ in 0..2 {
                    assert_eq!(0, uf.find(2));
                    assert_eq!(
                        UnionFind(vec![
                            UnionFindNode { par: 0, rank: 1 },
                            UnionFindNode { par: 0, rank: 0 },
                            UnionFindNode { par: 0, rank: 0 },
                            UnionFindNode { par: 3, rank: 0 },
                            UnionFindNode { par: 4, rank: 0 },
                        ]),
                        uf
                    );
                }
            }

            {
                let mut uf = uf.clone();
                assert_eq!(4, uf.find(4));
                assert_eq!(
                    UnionFind(vec![
                        UnionFindNode { par: 0, rank: 1 },
                        UnionFindNode { par: 0, rank: 0 },
                        UnionFindNode { par: 1, rank: 0 },
                        UnionFindNode { par: 3, rank: 0 },
                        UnionFindNode { par: 4, rank: 0 },
                    ]),
                    uf
                );
            }
        }

        #[test]
        fn unite() {
            let mut uf = UnionFind(vec![
                UnionFindNode { par: 0, rank: 0 },
                UnionFindNode { par: 1, rank: 0 },
                UnionFindNode { par: 2, rank: 0 },
                UnionFindNode { par: 3, rank: 0 },
                UnionFindNode { par: 4, rank: 0 },
            ]);

            uf.unite(2, 2);
            assert_eq!(
                UnionFind(vec![
                    UnionFindNode { par: 0, rank: 0 },
                    UnionFindNode { par: 1, rank: 0 },
                    UnionFindNode { par: 2, rank: 0 },
                    UnionFindNode { par: 3, rank: 0 },
                    UnionFindNode { par: 4, rank: 0 },
                ]),
                uf
            );

            for _ in 0..2 {
                uf.unite(0, 1);
                assert_eq!(
                    UnionFind(vec![
                        UnionFindNode { par: 0, rank: 1 },
                        UnionFindNode { par: 0, rank: 0 },
                        UnionFindNode { par: 2, rank: 0 },
                        UnionFindNode { par: 3, rank: 0 },
                        UnionFindNode { par: 4, rank: 0 },
                    ]),
                    uf
                );
            }

            uf.unite(3, 1);
            assert_eq!(
                UnionFind(vec![
                    UnionFindNode { par: 0, rank: 1 },
                    UnionFindNode { par: 0, rank: 0 },
                    UnionFindNode { par: 2, rank: 0 },
                    UnionFindNode { par: 0, rank: 0 },
                    UnionFindNode { par: 4, rank: 0 },
                ]),
                uf
            );

            uf.unite(0, 4);
            assert_eq!(
                UnionFind(vec![
                    UnionFindNode { par: 0, rank: 1 },
                    UnionFindNode { par: 0, rank: 0 },
                    UnionFindNode { par: 2, rank: 0 },
                    UnionFindNode { par: 0, rank: 0 },
                    UnionFindNode { par: 0, rank: 0 },
                ]),
                uf
            );
        }

        #[test]
        fn same() {
            let mut uf = UnionFind(vec![
                UnionFindNode { par: 0, rank: 0 },
                UnionFindNode { par: 0, rank: 1 },
                UnionFindNode { par: 2, rank: 0 },
                UnionFindNode { par: 3, rank: 0 },
                UnionFindNode { par: 4, rank: 0 },
            ]);

            assert_eq!(true, uf.same(0, 1));
            assert_eq!(true, uf.same(1, 0));
            assert_eq!(false, uf.same(1, 3));
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
    test1: "5 2\n5 3 1 4 2\n1 3\n5 4" => "2",
    test2: "3 2\n3 2 1\n1 2\n2 3" => "3",
    test3: "10 8\n5 3 6 8 7 10 9 1 2 4\n3 1\n4 1\n5 9\n2 5\n6 5\n3 5\n8 9\n7 9" => "8",
    test4: "5 1\n1 2 3 4 5\n1 5" => "5",
}
