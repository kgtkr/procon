extern crate core;

use std::cmp::{max, min};
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
    input!(input=>(h:usize w:usize){10;c_list:[i64]}{h;list:[i64]});
    let min_cost = warshall_floyd(graph::MatrixGraph(
        c_list
            .into_iter()
            .map(|x| x.into_iter().map(Some).collect::<Vec<_>>())
            .collect::<Vec<_>>(),
    ));
    let mut sum = 0;
    for i in 0..h {
        for j in 0..w {
            let x = list[i][j];
            if x != -1 && x != 1 {
                sum += min_cost[x as usize][1].unwrap();
            }
        }
    }
    sum.to_string()
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

    #[derive(PartialEq, Debug, Clone)]
    pub struct MazeID(pub Vec<Vec<Option<usize>>>);
    impl From<MazeID> for ListGraph {
        fn from(MazeID(maze): MazeID) -> ListGraph {
            if maze.len() == 0 {
                return ListGraph(Vec::new());
            }

            let h = maze.len();
            let w = maze[0].len();

            let mut graph = Vec::new();
            for y in 0..h {
                for x in 0..w {
                    if let Some(_) = maze[y][x] {
                        let mut edges = Vec::new();
                        if y != 0 {
                            if let Some(to) = maze[y - 1][x] {
                                edges.push((to, 1));
                            }
                        }
                        if x != 0 {
                            if let Some(to) = maze[y][x - 1] {
                                edges.push((to, 1));
                            }
                        }
                        if x != w - 1 {
                            if let Some(to) = maze[y][x + 1] {
                                edges.push((to, 1));
                            }
                        }
                        if y != h - 1 {
                            if let Some(to) = maze[y + 1][x] {
                                edges.push((to, 1));
                            }
                        }
                        graph.push(edges);
                    }
                }
            }
            ListGraph::from(graph)
        }
    }

    //迷路
    #[derive(PartialEq, Debug, Clone)]
    pub struct Maze(pub Vec<Vec<bool>>);

    impl From<Maze> for MazeID {
        fn from(Maze(maze): Maze) -> MazeID {
            let mut id = 0;
            MazeID(
                maze.into_iter()
                    .map(|vec| {
                        vec.into_iter()
                            .map(|x| {
                                if x {
                                    let res: Option<usize> = Some(id);
                                    id += 1;
                                    res
                                } else {
                                    None
                                }
                            })
                            .collect::<Vec<_>>()
                    })
                    .collect::<Vec<_>>(),
            )
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
                    vec![(0, 1, 1), (0, 3, 3), (1, 0, 10), (1, 1, 1), (2, 2, 5)],
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
                    vec![(0, 1, 1), (0, 3, 3), (1, 0, 10), (1, 1, 1), (2, 2, 5)],
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
        fn maze_id_to_list() {
            assert_eq!(
                ListGraph(vec![
                    vec![(0, 2, 1)],
                    vec![(1, 4, 1)],
                    vec![(2, 0, 1), (2, 3, 1), (2, 6, 1)],
                    vec![(3, 2, 1), (3, 4, 1)],
                    vec![(4, 1, 1), (4, 3, 1), (4, 5, 1), (4, 7, 1)],
                    vec![(5, 4, 1)],
                    vec![(6, 2, 1)],
                    vec![(7, 4, 1)],
                ]),
                ListGraph::from(MazeID::from(Maze(vec![
                    vec![true, false, true, false],
                    vec![true, true, true, true],
                    vec![true, false, true, false],
                ])))
            );
        }

        #[test]
        fn maze_to_maze_id() {
            assert_eq!(
                MazeID(vec![
                    vec![Some(0), None, Some(1), None],
                    vec![Some(2), Some(3), Some(4), Some(5)],
                    vec![Some(6), None, Some(7), None],
                ]),
                MazeID::from(Maze(vec![
                    vec![true, false, true, false],
                    vec![true, true, true, true],
                    vec![true, false, true, false],
                ]))
            );
        }
    }
}

fn warshall_floyd(
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
    test1: "2 4\n0 9 9 9 9 9 9 9 9 9\n9 0 9 9 9 9 9 9 9 9\n9 9 0 9 9 9 9 9 9 9\n9 9 9 0 9 9 9 9 9 9\n9 9 9 9 0 9 9 9 9 2\n9 9 9 9 9 0 9 9 9 9\n9 9 9 9 9 9 0 9 9 9\n9 9 9 9 9 9 9 0 9 9\n9 9 9 9 2 9 9 9 0 9\n9 2 9 9 9 9 9 9 9 0\n-1 -1 -1 -1\n8 1 1 8" => "12",
    test2: "5 5\n0 999 999 999 999 999 999 999 999 999\n999 0 999 999 999 999 999 999 999 999\n999 999 0 999 999 999 999 999 999 999\n999 999 999 0 999 999 999 999 999 999\n999 999 999 999 0 999 999 999 999 999\n999 999 999 999 999 0 999 999 999 999\n999 999 999 999 999 999 0 999 999 999\n999 999 999 999 999 999 999 0 999 999\n999 999 999 999 999 999 999 999 0 999\n999 999 999 999 999 999 999 999 999 0\n1 1 1 1 1\n1 1 1 1 1\n1 1 1 1 1\n1 1 1 1 1\n1 1 1 1 1" => "0",
    test3: "3 5\n0 4 3 6 2 7 2 5 3 3\n4 0 5 3 7 5 3 7 2 7\n5 7 0 7 2 9 3 2 9 1\n3 6 2 0 2 4 6 4 2 3\n3 5 7 4 0 6 9 7 6 7\n9 8 5 2 2 0 4 7 6 5\n5 4 6 3 2 3 0 5 4 3\n3 6 2 3 4 2 4 0 8 9\n4 6 5 4 3 5 3 2 0 8\n2 1 3 4 5 7 8 6 4 0\n3 5 2 6 1\n2 5 3 2 1\n6 9 2 5 6" => "47",
}
