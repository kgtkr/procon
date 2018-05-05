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
    input!(input=>(h:usize w:usize){h;list:#});
    let list = list.into_iter()
        .map(|x| x.chars().map(|x| x == '#').collect::<Vec<_>>())
        .collect::<Vec<_>>();
    let graph = graph::ListGraph::from(list);
    //グラフの長さ
    let graph_len = graph.0.len();
    //有効ノードの長さ
    let ok_nodes_len = input.chars().filter(|&c| c == '#').count();
    //長さが0でないノードの一覧
    let ok_nodes = graph
        .clone()
        .0
        .into_iter()
        .enumerate()
        .filter(|&(_, ref l)| l.len() != 0)
        .collect::<Vec<_>>();
    if ok_nodes_len == 0 {
        "Yes"
    } else {
        if ok_nodes.len() == 0 {
            "No"
        } else {
            let mut dij = dijsktra::Dijsktra::new(graph);
            println!("{:?}", ok_nodes);
            let not = dij.nodes.into_iter().filter(|x| x.cost.is_none()).count();
            if not <= (graph_len - ok_nodes_len) {
                "Yes"
            } else {
                "No"
            }
        }
    }.to_string()
}

mod dijsktra {
    use std::cmp::Ordering;
    use std::collections::BinaryHeap;
    use graph;

    #[derive(PartialEq, Debug, Clone)]
    pub struct Node {
        pub edges: Vec<graph::Edge>,
        pub done: bool,
        pub cost: Option<graph::Cost>,
        pub before: Option<usize>,
    }

    impl Node {
        pub fn new(node: graph::Node) -> Node {
            Node {
                edges: node,
                done: false,
                cost: None,
                before: None,
            }
        }

        pub fn set_start(&mut self) {
            self.cost = Some(0);
        }
    }

    #[derive(PartialEq, Debug, Clone)]
    pub struct Dijsktra {
        pub nodes: Vec<Node>,
    }

    impl Dijsktra {
        pub fn new(graph::ListGraph(nodes): graph::ListGraph) -> Dijsktra {
            Dijsktra {
                nodes: nodes
                    .into_iter()
                    .map(|node| Node::new(node))
                    .collect::<Vec<_>>(),
            }
        }

        pub fn cost(&self, node: graph::NodeId) -> Option<graph::Cost> {
            self.nodes[node].cost
        }

        pub fn dijsktra(&mut self, start: graph::NodeId) {
            let mut heap = BinaryHeap::new();
            heap.push(State {
                cost: 0,
                node: start,
            });
            self.nodes[start].set_start();

            while let Some(done_node) = heap.pop().map(|State { node, cost: _ }| node) {
                self.nodes[done_node].done = true;
                for (_, edge_to, edge_cost) in self.nodes[done_node].edges.clone() {
                    let cost = self.nodes[done_node].cost.unwrap() + edge_cost;
                    if self.nodes[edge_to]
                        .cost
                        .map(|to_cost| cost < to_cost)
                        .unwrap_or(true)
                    {
                        self.nodes[edge_to].cost = Some(cost);
                        self.nodes[edge_to].before = Some(done_node);
                        heap.push(State {
                            node: edge_to,
                            cost: cost,
                        });
                    }
                }
            }
        }

        pub fn path(&self, goal: usize) -> Vec<usize> {
            let mut path = Vec::new();
            path.push(goal);
            let mut current = goal;
            while let Some(node) = self.nodes[current].before {
                path.push(node);
                current = node;
            }
            path.reverse();
            path
        }
    }

    #[derive(Copy, Clone, Eq, PartialEq)]
    struct State {
        cost: graph::Cost,
        node: graph::NodeId,
    }

    impl Ord for State {
        fn cmp(&self, other: &State) -> Ordering {
            other.cost.cmp(&self.cost)
        }
    }

    impl PartialOrd for State {
        fn partial_cmp(&self, other: &State) -> Option<Ordering> {
            Some(self.cmp(other))
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn test1() {
            let mut graph = vec![
                vec![(2, 10), (1, 1)],
                vec![(3, 2)],
                vec![(1, 1), (3, 3), (4, 1)],
                vec![(0, 7), (4, 2)],
                vec![],
            ];

            let mut dij = Dijsktra::new(graph.into());

            dij.dijsktra(0);

            assert_eq!(Some(0), dij.cost(0));
            assert_eq!(Some(1), dij.cost(1));
            assert_eq!(Some(10), dij.cost(2));
            assert_eq!(Some(3), dij.cost(3));
            assert_eq!(Some(5), dij.cost(4));

            assert_eq!(vec![0], dij.path(0));
            assert_eq!(vec![0, 1], dij.path(1));
            assert_eq!(vec![0, 2], dij.path(2));
            assert_eq!(vec![0, 1, 3], dij.path(3));
            assert_eq!(vec![0, 1, 3, 4], dij.path(4));
        }
    }
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
    test1: "3 3\n.#.\n###\n.#." => "Yes",
    test2: "5 5\n#.#.#\n.#.#.\n#.#.#\n.#.#.\n#.#.#" => "No",
    test3: "11 11\n...#####...\n.##.....##.\n#..##.##..#\n#..##.##..#\n#.........#\n#...###...#\n.#########.\n.#.#.#.#.#.\n##.#.#.#.##\n..##.#.##..\n.##..#..##." => "Yes",
}
