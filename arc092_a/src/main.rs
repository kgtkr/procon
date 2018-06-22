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
    input!(input=>(n:usize){n;red:(i64,i64)}{n;blue:(i64,i64)});
    let mut v = Vec::new();
    for (i, (x, y)) in red.into_iter().enumerate() {
        for (j, (a, b)) in blue.clone().into_iter().enumerate() {
            if x < a && y < b {
                v.push((i, j));
            }
        }
    }

    flow::max_match(n, n, v).to_string()
}

mod flow {
    use std::cmp::min;

    #[derive(PartialEq, Debug, Clone)]
    struct Edge {
        pub to: usize,
        pub cap: i64,
        pub rev: usize,
    }

    struct FordFulkerson(Vec<(Vec<Edge>, bool)>);

    impl FordFulkerson {
        pub fn new(n: usize) -> FordFulkerson {
            let v = (Vec::new(), false);
            let mut nodes = Vec::with_capacity(n);
            nodes.resize(n, v);
            FordFulkerson(nodes)
        }

        pub fn add_edge(&mut self, from: usize, to: usize, cap: i64) {
            let f_size = self.0[from].0.len();
            let t_size = self.0[to].0.len();
            self.0[from].0.push(Edge {
                to: to,
                cap: cap,
                rev: t_size,
            });
            self.0[to].0.push(Edge {
                to: from,
                cap: 0,
                rev: f_size,
            });
        }

        pub fn dfs(&mut self, v: usize, t: usize, f: i64) -> i64 {
            if v == t {
                return f;
            }
            self.0[v].1 = true;
            for i in 0..self.0[v].0.len() {
                if !self.0[self.0[v].0[i].to].1 && self.0[v].0[i].cap > 0 {
                    let d = {
                        let x = self.0[v].0[i].to;
                        let y = min(f, self.0[v].0[i].cap);
                        self.dfs(x, t, y)
                    };
                    if d > 0 {
                        self.0[v].0[i].cap -= d;
                        {
                            let x = self.0[v].0[i].to;
                            let y = self.0[v].0[i].rev;
                            self.0[x].0[y].cap += d;
                        }
                        return d;
                    }
                }
            }
            0
        }

        pub fn max_flow(&mut self, s: usize, t: usize) -> i64 {
            let mut flow = 0;
            loop {
                for i in 0..self.0.len() {
                    self.0[i].1 = false;
                }
                let f = self.dfs(s, t, <i64>::max_value());
                if f == 0 {
                    return flow;
                }
                flow += f;
            }
        }
    }

    pub fn max_match(a: usize, b: usize, path: Vec<(usize, usize)>) -> i64 {
        //スタート、グループa、グループb、ゴール
        let mut flow = FordFulkerson::new(a + b + 2);

        //スタートとaを結ぶ
        for i in 1..a + 1 {
            flow.add_edge(0, i, 1);
        }

        //aとbを結ぶ
        for (x, y) in path {
            flow.add_edge(x + 1, y + a + 1, 1);
        }

        //bとゴールを結ぶ
        for i in a + 1..a + b + 1 {
            flow.add_edge(i, a + b + 1, 1);
        }

        flow.max_flow(0, a + b + 1)
    }

    mod tests {
        use super::*;

        #[test]
        fn test1() {
            let mut flow = FordFulkerson::new(5);
            flow.add_edge(0, 1, 10);
            flow.add_edge(0, 2, 2);
            flow.add_edge(1, 2, 6);
            flow.add_edge(1, 3, 6);
            flow.add_edge(2, 4, 5);
            flow.add_edge(3, 2, 3);
            flow.add_edge(3, 4, 8);
            assert_eq!(11, flow.max_flow(0, 4));
        }

        #[test]
        fn test2() {
            assert_eq!(
                3,
                max_match(3, 3, vec![(0, 0), (0, 1), (1, 0), (1, 2), (2, 1)])
            );
            assert_eq!(
                2,
                max_match(3, 4, vec![(0, 0), (0, 1), (0, 2), (0, 3), (1, 0), (2, 0)])
            );
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
    test1: "3\n2 0\n3 1\n1 3\n4 2\n0 4\n5 5" => "2",
    test2: "3\n0 0\n1 1\n5 2\n2 3\n3 4\n4 5" => "2",
    test3: "2\n2 2\n3 3\n0 0\n1 1" => "0",
    test4: "5\n0 0\n7 3\n2 2\n4 8\n1 6\n8 5\n6 9\n5 4\n9 1\n3 7" => "5",
    test5: "5\n0 0\n1 1\n5 5\n6 6\n7 7\n2 2\n3 3\n4 4\n8 8\n9 9" => "4",
}
