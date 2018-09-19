extern crate core;

use std::io::{self, Read};
use std::collections::HashMap;

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
    input!(input=>(h:usize w:usize s:# t:#){h;list:#});
    let s=s.chars().next().unwrap();
    let t=t.chars().next().unwrap();
    let list = list.into_iter()
        .map(|x| x.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>();
    let nodes=list.clone()
        .into_iter()
        .enumerate()
        .flat_map(|(y, s)| {
            s.into_iter()
                .enumerate()
                .filter(|&(x, c)| is_az(c))
                .zip(std::iter::repeat(y))
                .map(|((x, c),y)| {
                    let top = y>=2&&list.get(y - 2).and_then(|i| i.get(x)) == Some(&'|');
                    let bottom = list.get(y + 2).and_then(|i| i.get(x)) == Some(&'|');
                    let left = x>=2&&list.get(y).and_then(|i| i.get(x - 2)) == Some(&'-');
                    let right = list.get(y).and_then(|i| i.get(x + 2)) == Some(&'-');
                    (c, y, x, top, bottom, left, right)
                })
        })
        .enumerate()
        .collect::<Vec<_>>();

    //ID to Name
    let id2name=nodes.clone().into_iter().map(|(_,(c,_,_,_,_,_,_))|c).collect::<Vec<_>>();
    let name2id={
        let mut map=HashMap::new();
        for (i,x) in id2name.clone().into_iter().enumerate(){
            map.insert(x,i);
        }
        map
    };

    
    let arena=graph::NodeArena{
        arena:nodes.into_iter().map(|(id, (c, y, x, top, bottom, left, right))| {
            let mut paths=Vec::new();
            if top {
                paths.push(*name2id.get(&(0..y).rev().map(|i|list[i][x]).find(|&c|is_az(c)).unwrap()).unwrap());
            }

            if bottom {
                paths.push(*name2id.get(&((y+1)..h).map(|i|list[i][x]).find(|&c|is_az(c)).unwrap()).unwrap());
            }

            if left {
                paths.push(*name2id.get(&(0..x).rev().map(|i|list[y][i]).find(|&c|is_az(c)).unwrap()).unwrap());
            }

            if right {
                paths.push(*name2id.get(&((x+1)..w).map(|i|list[y][i]).find(|&c|is_az(c)).unwrap()).unwrap());
            }

            graph::Node{
                id:id,
                edges:paths.into_iter().map(|to|graph::Edge{to:to,cost:1}).collect()
            }
        })
        .collect()
    };

    let mut dij = dijsktra::Dijsktra::new(arena);
    dij.dijsktra(*name2id.get(&s).unwrap());
    dij.cost(*name2id.get(&t).unwrap()).unwrap().to_string()
}

fn is_az(c: char) -> bool {
    'A' <= c && c <= 'Z'
}

mod graph {
    //http://agtn.hatenablog.com/entry/2017/01/16/151745

    pub type NodeId = usize;

    pub struct NodeArena {
        pub arena: Vec<Node>,
    }

    impl NodeArena {
        pub fn new() -> NodeArena {
            NodeArena { arena: Vec::new() }
        }

        pub fn alloc(&mut self) -> NodeId {
            let id = self.arena.len();
            let node = Node {
                id: id,
                edges: Vec::new(),
            };
            self.arena.push(node);
            id
        }

        pub fn add_edge(&mut self, node: NodeId, to: NodeId, cost: i32) {
            self.arena[node].edges.push(Edge { to: to, cost: cost });
        }

        pub fn get(&self, id: NodeId) -> &Node {
            &self.arena[id]
        }
        pub fn get_mut(&mut self, id: NodeId) -> &mut Node {
            &mut self.arena[id]
        }
    }

    #[derive(PartialEq, Debug, Clone)]
    pub struct Node {
        pub id: NodeId,
        pub edges: Vec<Edge>,
    }

    #[derive(PartialEq, Debug, Clone)]
    pub struct Edge {
        pub to: NodeId,
        pub cost: i32,
    }
}

mod dijsktra {
    use std::cmp::Ordering;
    use std::collections::BinaryHeap;
    use graph;

    #[derive(PartialEq, Debug, Clone)]
    pub struct Node {
        pub edges: Vec<graph::Edge>,
        pub done: bool,
        pub cost: Option<i32>,
        pub before: Option<usize>,
    }

    impl Node {
        pub fn new(node: graph::Node) -> Node {
            Node {
                edges: node.edges,
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
        pub fn new(nodes: graph::NodeArena) -> Dijsktra {
            Dijsktra {
                nodes: nodes
                    .arena
                    .into_iter()
                    .map(|node| Node::new(node))
                    .collect::<Vec<_>>(),
            }
        }

        pub fn cost(&self, node: graph::NodeId) -> Option<i32> {
            self.nodes[node].cost
        }

        pub fn dijsktra(&mut self, start: graph::NodeId) {
            let mut heap = BinaryHeap::new();
            heap.push(State {
                cost: 0,
                node: start,
            });
            self.nodes[start].set_start();

            self.dijsktra_r(&mut heap);
        }
        //スタートのコストを0とすること
        fn dijsktra_r(&mut self, heap: &mut BinaryHeap<State>) {
            let done_node = heap.pop().map(|State { node, cost: _ }| node);

            if let Some(done_node) = done_node {
                self.nodes[done_node].done = true;
                for edge in self.nodes[done_node].edges.clone() {
                    let cost = self.nodes[done_node].cost.unwrap() + edge.cost;
                    if self.nodes[edge.to]
                        .cost
                        .map(|to_cost| cost < to_cost)
                        .unwrap_or(true)
                    {
                        self.nodes[edge.to].cost = Some(cost);
                        self.nodes[edge.to].before = Some(done_node);
                        heap.push(State {
                            node: edge.to,
                            cost: cost,
                        });
                    }
                }

                self.dijsktra_r(heap);
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
        cost: i32,
        node: usize,
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
    test1: "14 16 A L 
ooo.....ooo.....
oAo-----oHo.....
ooo.....ooo..ooo
.|.......|...oLo
ooo..ooo.|...ooo
oKo--oYo.|....|.
ooo..ooo.|....|.
.|....|.ooo...|.
.|....|.oGo...|.
.|....|.ooo...|.
.|....|.......|.
ooo..ooo.....ooo
oFo--oXo-----oEo
ooo..ooo.....ooo" => "5",
    test2: "21 17 F L
.................
.....ooo.....ooo.
.....oAo-----oBo.
.....ooo.....ooo.
......|.......|..
.ooo..|..ooo..|..
.oCo..|..oDo.ooo.
.ooo.ooo.ooo.oEo.
..|..oFo..|..ooo.
..|..ooo..|...|..
..|...|...|...|..
..|...|...|...|..
..|...|...|...|..
.ooo.ooo.ooo..|..
.oGo-oHo-oIo..|..
.ooo.ooo.ooo..|..
..|...........|..
.ooo...ooo...ooo.
.oJo---oKo---oLo.
.ooo...ooo...ooo.
................." => "4",
}
