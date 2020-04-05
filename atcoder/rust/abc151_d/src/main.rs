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
    input!(input=>(h:usize w:usize){h;maze:#});
    let graph = graph::ListGraph::from(graph::MazeID::from(graph::Maze(
        maze.into_iter()
            .map(|x| x.chars().map(|x| x == '.').collect::<Vec<_>>())
            .collect::<Vec<_>>(),
    )));
    let mut max = 0;
    for i in 0..graph.0.len() {
        for j in 0..graph.0.len() {
            let step = bfs(&graph, i, j).unwrap().len() - 1;
            if max < step {
                max = step;
            }
        }
    }
    max.to_string()
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
    test1: "3 3\n...\n...\n...\n" => "4\n",
    test2: "3 5\n...#.\n.#.#.\n.#...\n" => "10\n",
}

pub fn bfs(
    &graph::ListGraph(ref graph): &graph::ListGraph,
    start: graph::NodeId,
    goal: graph::NodeId,
) -> Option<Vec<graph::NodeId>> {
    let mut visited = Vec::with_capacity(graph.len());
    visited.resize(graph.len(), None);
    let mut n = start;
    // Queue に初期値を積む
    let mut queue = vec![n];
    while queue.len() > 0 {
        // queueから取り出す
        n = queue[0];
        queue.remove(0);
        // 行けるノード
        for &(_, next_node, _) in &graph[n] {
            if visited[next_node].is_none() {
                visited[next_node] = Some(n);
                // Queueに積む
                queue.push(next_node)
            }

            if next_node == goal {
                visited[start] = None;
                let mut path = vec![goal];
                let mut c = visited[goal];
                while let Some(id) = c {
                    path.push(id);
                    c = visited[id];
                }
                path.reverse();
                return Some(path);
            }
        }
    }
    None
}

mod graph {

    // lib
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
}
