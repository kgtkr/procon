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
    let output = run(input.trim().to_string());
    println!("{}", output);
}

fn run(input: String) -> String {
    input!(input=>(n:usize m:usize){m;list:(usize,usize,i64)});
    let list = list.into_iter()
        .flat_map(|(v1, v2, l)| vec![(v1 - 1, v2 - 1, l), (v2 - 1, v1 - 1, l)])
        .collect::<Vec<_>>();

    //0からの辺を除くグラフ
    let list1 = list.clone()
        .into_iter()
        .filter(|&(v1, v2, _)| v1 != 0 && v2 != 0)
        .collect::<Vec<_>>();

    //0からの辺
    let list2 = graph::ListGraph::from(graph::FlatGraph(n, list)).0[0]
        .clone()
        .into_iter()
        .map(|(_, a, b)| (a, b))
        .collect::<Vec<_>>();

    let wf = warshall_floyd::warshall_floyd(graph::MatrixGraph::from(graph::FlatGraph(n, list1)));

    let mut min = None;
    //2つの辺の選び方
    for &(a, a_cost) in &list2 {
        for &(b, b_cost) in &list2 {
            if a != b {
                if let Some(cost) = wf[a][b] {
                    let cost = cost + a_cost + b_cost;
                    min = match min {
                        None => Some(cost),
                        Some(min) if min > cost => Some(cost),
                        _ => min,
                    };
                }
            }
        }
    }

    min.unwrap_or(-1).to_string()
}

macro_rules! tests {
    ($($name:ident: $input:expr=>$output:expr,)*) => {
        mod tests {
            $(
                #[test]
                fn $name() {
                    assert_eq!(super::run($input.to_string()), $output.to_string());
                }
            )*
        }
    }
}

tests! {
    test1: "5 7\n1 2 2\n1 4 1\n2 3 7\n1 5 12\n3 5 2\n2 5 3\n3 4 5" => "13",
    test2: "5 4\n1 2 1\n1 3 1\n1 4 1\n1 5 1" => "-1",
    test3: "10 12\n1 4 3\n1 9 1\n2 5 4\n2 6 1\n3 7 5\n3 10 9\n4 7 2\n5 6 6\n5 8 5\n6 8 3\n7 9 5\n8 10 8" => "11",
}

mod graph {
    pub type NodeId = usize;

    pub type Cost = i64;

    pub type Node = Vec<Edge>;

    pub type Edge = (NodeId, NodeId, Cost);

    #[derive(PartialEq, Debug, Clone)]
    pub struct MatrixGraph(pub Vec<Vec<Option<Cost>>>);

    impl From<Vec<Vec<Option<Cost>>>> for MatrixGraph {
        fn from(data: Vec<Vec<Option<Cost>>>) -> MatrixGraph {
            MatrixGraph(data)
        }
    }

    impl From<ListGraph> for MatrixGraph {
        fn from(graph: ListGraph) -> MatrixGraph {
            FlatGraph::from(graph).into()
        }
    }

    impl From<FlatGraph> for MatrixGraph {
        fn from(FlatGraph(len, data): FlatGraph) -> MatrixGraph {
            let mut vec = Vec::with_capacity(len);
            vec.resize(len, {
                let mut v = Vec::with_capacity(len);
                v.resize(len, None);
                v
            });

            for (from, to, cost) in data {
                vec[from][to] = Some(cost);
            }

            vec.into()
        }
    }

    #[derive(PartialEq, Debug, Clone)]
    pub struct ListGraph(pub Vec<Vec<Edge>>);

    impl From<Vec<Vec<(NodeId, Cost)>>> for ListGraph {
        fn from(data: Vec<Vec<(NodeId, Cost)>>) -> ListGraph {
            ListGraph(
                data.into_iter()
                    .enumerate()
                    .map(|(from, edges)| {
                        edges
                            .into_iter()
                            .map(|(to, cost)| (from, to, cost))
                            .collect()
                    })
                    .collect(),
            )
        }
    }

    impl From<Vec<Vec<Edge>>> for ListGraph {
        fn from(data: Vec<Vec<Edge>>) -> ListGraph {
            ListGraph(data)
        }
    }

    impl From<FlatGraph> for ListGraph {
        fn from(FlatGraph(len, data): FlatGraph) -> ListGraph {
            let mut vec = Vec::with_capacity(len);
            vec.resize(len, Vec::new());

            for (from, to, cost) in data {
                vec[from].push((from, to, cost));
            }

            ListGraph(vec)
        }
    }

    impl From<MatrixGraph> for ListGraph {
        fn from(graph: MatrixGraph) -> ListGraph {
            FlatGraph::from(graph).into()
        }
    }

    #[derive(PartialEq, Debug, Clone)]
    pub struct FlatGraph(pub usize, pub Vec<Edge>);

    impl From<ListGraph> for FlatGraph {
        fn from(ListGraph(data): ListGraph) -> FlatGraph {
            let len = data.len();
            (len, data.into_iter().flat_map(|x| x).collect::<Vec<_>>()).into()
        }
    }

    impl From<(usize, Vec<(NodeId, NodeId, Cost)>)> for FlatGraph {
        fn from((len, data): (usize, Vec<(NodeId, NodeId, Cost)>)) -> FlatGraph {
            FlatGraph(len, data)
        }
    }

    impl From<MatrixGraph> for FlatGraph {
        fn from(MatrixGraph(data): MatrixGraph) -> FlatGraph {
            let mut vec = Vec::new();
            let len = data.len();

            for (from, v) in data.into_iter().enumerate() {
                for (to, cost) in v.into_iter().enumerate() {
                    if let Some(cost) = cost {
                        vec.push((from, to, cost));
                    }
                }
            }

            (len, vec).into()
        }
    }

    //迷路
    pub type Maze = Vec<Vec<bool>>;

    impl From<Maze> for ListGraph {
        fn from(maze: Maze) -> ListGraph {
            if maze.len() == 0 {
                return Vec::<Vec<(NodeId, Cost)>>::new().into();
            }

            let h = maze.len();
            let w = maze[0].len();

            let mut graph = Vec::new();
            for y in 0..h {
                for x in 0..w {
                    if maze[y][x] {
                        let mut edges = Vec::new();
                        if y != 0 && maze[y - 1][x] {
                            edges.push((x + (y - 1) * w, 1));
                        }
                        if x != 0 && maze[y][x - 1] {
                            edges.push(((x - 1) + y * w, 1));
                        }
                        if x != w - 1 && maze[y][x + 1] {
                            edges.push(((x + 1) + y * w, 1));
                        }
                        if y != h - 1 && maze[y + 1][x] {
                            edges.push((x + (y + 1) * w, 1));
                        }
                        graph.push(edges);
                    } else {
                        graph.push(Vec::new());
                    }
                }
            }
            graph.into()
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn list_to_matrix() {
            assert_eq!(
                MatrixGraph(vec![
                    vec![None, Some(1), None, Some(3)],
                    vec![Some(10), Some(1), None, None],
                    vec![None, None, Some(5), None],
                    vec![None, None, None, None],
                ]),
                MatrixGraph::from(ListGraph(vec![
                    vec![(0, 1, 1), (0, 3, 3)],
                    vec![(1, 0, 10), (1, 1, 1)],
                    vec![(2, 2, 5)],
                    vec![],
                ]))
            );
        }

        #[test]
        fn flat_to_matrix() {
            assert_eq!(
                MatrixGraph(vec![
                    vec![None, Some(1), None, Some(3)],
                    vec![Some(10), Some(1), None, None],
                    vec![None, None, Some(5), None],
                    vec![None, None, None, None],
                ]),
                MatrixGraph::from(FlatGraph(
                    4,
                    vec![(0, 1, 1), (0, 3, 3), (1, 0, 10), (1, 1, 1), (2, 2, 5)]
                ))
            );
        }

        #[test]
        fn data_to_list() {
            assert_eq!(
                ListGraph(vec![
                    vec![(0, 1, 1), (0, 3, 3)],
                    vec![(1, 0, 10), (1, 1, 1)],
                    vec![(2, 2, 5)],
                    vec![],
                ]),
                ListGraph::from(vec![
                    vec![(1, 1), (3, 3)],
                    vec![(0, 10), (1, 1)],
                    vec![(2, 5)],
                    vec![],
                ])
            );
        }

        #[test]
        fn flat_to_list() {
            assert_eq!(
                ListGraph(vec![
                    vec![(0, 1, 1), (0, 3, 3)],
                    vec![(1, 0, 10), (1, 1, 1)],
                    vec![(2, 2, 5)],
                    vec![],
                ]),
                ListGraph::from(FlatGraph(
                    4,
                    vec![(0, 1, 1), (0, 3, 3), (1, 0, 10), (1, 1, 1), (2, 2, 5)]
                ))
            );
        }

        #[test]
        fn list_to_flat() {
            assert_eq!(
                FlatGraph(
                    4,
                    vec![(0, 1, 1), (0, 3, 3), (1, 0, 10), (1, 1, 1), (2, 2, 5)]
                ),
                FlatGraph::from(ListGraph(vec![
                    vec![(0, 1, 1), (0, 3, 3)],
                    vec![(1, 0, 10), (1, 1, 1)],
                    vec![(2, 2, 5)],
                    vec![],
                ]))
            );
        }

        #[test]
        fn mtrix_to_flat() {
            assert_eq!(
                FlatGraph(
                    4,
                    vec![(0, 1, 1), (0, 3, 3), (1, 0, 10), (1, 1, 1), (2, 2, 5)]
                ),
                FlatGraph::from(MatrixGraph(vec![
                    vec![None, Some(1), None, Some(3)],
                    vec![Some(10), Some(1), None, None],
                    vec![None, None, Some(5), None],
                    vec![None, None, None, None],
                ]))
            );
        }

        #[test]
        fn maze_to_list() {
            assert_eq!(
                ListGraph(vec![
                    vec![(0, 4, 1)],
                    vec![],
                    vec![(2, 6, 1)],
                    vec![],
                    vec![(4, 0, 1), (4, 5, 1), (4, 8, 1)],
                    vec![(5, 4, 1), (5, 6, 1)],
                    vec![(6, 2, 1), (6, 5, 1), (6, 7, 1), (6, 10, 1)],
                    vec![(7, 6, 1)],
                    vec![(8, 4, 1)],
                    vec![],
                    vec![(10, 6, 1)],
                    vec![],
                ]),
                ListGraph::from(vec![
                    vec![true, false, true, false],
                    vec![true, true, true, true],
                    vec![true, false, true, false],
                ])
            );
        }
    }
}

mod warshall_floyd {
    use graph;
    use std::cmp::min;

    pub fn warshall_floyd(
        graph::MatrixGraph(mut vec): graph::MatrixGraph,
    ) -> Vec<Vec<Option<graph::Cost>>> {
        for k in 0..vec.len() {
            for i in 0..vec.len() {
                for j in 0..vec.len() {
                    let now = vec[i][j];
                    let new = match (vec[i][k], vec[k][j]) {
                        (Some(a), Some(b)) => Some(a + b),
                        _ => None,
                    };
                    vec[i][j] = match (now, new) {
                        (Some(a), Some(b)) => Some(min(a, b)),
                        (None, Some(x)) => Some(x),
                        (Some(x), None) => Some(x),
                        (None, None) => None,
                    };
                }
            }
        }

        vec
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn test1() {
            let mut graph = graph::MatrixGraph::from(graph::ListGraph::from(vec![
                vec![(2, 10), (1, 1)],
                vec![(3, 2)],
                vec![(1, 1), (3, 3), (4, 1)],
                vec![(0, 7), (4, 2)],
                vec![],
            ]));

            assert_eq!(
                vec![
                    vec![Some(10), Some(1), Some(10), Some(3), Some(5)],
                    vec![Some(9), Some(10), Some(19), Some(2), Some(4)],
                    vec![Some(10), Some(1), Some(20), Some(3), Some(1)],
                    vec![Some(7), Some(8), Some(17), Some(10), Some(2)],
                    vec![None, None, None, None, None],
                ],
                warshall_floyd(graph)
            );
        }
    }
}
