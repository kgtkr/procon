extern crate core;

use std::io::{self, Read};

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = run(input.trim().to_string());
    println!("{}", output);
}

#[derive(Debug, PartialEq, Clone)]
struct Vec2 {
    x: f64,
    y: f64,
}

#[derive(Debug, PartialEq, Clone)]
struct Circle {
    v: Vec2,
    r: f64,
}

fn run(input: String) -> String {
    let (start, end) = {
        let l = input
            .split("\n")
            .nth(0)
            .unwrap()
            .split_whitespace()
            .map(|x| x.parse::<f64>().unwrap())
            .collect::<Vec<_>>();
        (
            v_to_c(Vec2 { x: l[0], y: l[1] }),
            v_to_c(Vec2 { x: l[2], y: l[3] }),
        )
    };
    let list = {
        let mut a = input
            .split("\n")
            .skip(2)
            .map(|x| {
                let l = x.split_whitespace()
                    .map(|x| x.parse::<f64>().unwrap())
                    .collect::<Vec<_>>();
                Circle {
                    v: Vec2 { x: l[0], y: l[1] },
                    r: l[2],
                }
            })
            .collect::<Vec<_>>();
        a.insert(0, start);
        a.push(end);
        a
    };

    let mut nodes = list.iter()
        .enumerate()
        .map(|(i, x)| (x, graph::Node::new(i)))
        .collect::<Vec<_>>();

    for i in 0..nodes.len() {
        for j in 0..nodes.len() {
            if i != j {
                let id = nodes[j].1.id;
                let len = len_c(nodes[i].0, nodes[j].0);
                nodes[i].1.add_node(id, len);
            }
        }
    }
    let nodes = nodes.into_iter().map(|(_, x)| x).collect::<Vec<_>>();

    let (sum, _) = graph::min_route(nodes);

    sum.to_string()
}

fn v_to_c(v: Vec2) -> Circle {
    Circle {
        v: v.clone(),
        r: 0.0,
    }
}

fn len_c(a: &Circle, b: &Circle) -> f64 {
    (len_v(&a.v, &b.v) - a.r - b.r).max(0.0)
}

fn len_v(a: &Vec2, b: &Vec2) -> f64 {
    ((b.x - a.x).powi(2) + (b.y - a.y).powi(2)).sqrt()
}

#[test]
fn test() {
    let tests = vec![
        (
            "-2 -2 2 2
1
0 0 1",
            "3.6568542494923806",
        ),
        (
            "-2 0 2 0
2
-1 0 2
1 0 2",
            "0",
        ),
        (
            "4 -2 -2 4
3
0 0 2
4 0 1
0 4 1",
            "4",
        ),
    ];
    for (i, (input, output)) in tests.into_iter().enumerate() {
        println!("test:{}", i);
        assert_eq!(run(input.to_string()), output.to_string());
    }
}

//https://qiita.com/edo_m18/items/0588d290a19f2afc0a84
mod graph {
    #[derive(Debug, PartialEq, Clone)]
    pub struct Node {
        pub edges: Vec<(usize, f64)>,
        pub done: bool,
        pub cost: f64,
        pub id: usize,
        pub previous_node: Option<usize>,
    }

    impl Node {
        pub fn new(id: usize) -> Node {
            Node {
                edges: Vec::new(),
                done: false,
                cost: -1.0,
                id: id,
                previous_node: None,
            }
        }

        pub fn add_node(&mut self, id: usize, cost: f64) {
            self.edges.push((id, cost));
        }
    }

    pub fn min_route(mut nodes: Vec<Node>) -> (f64, Vec<Node>) {
        nodes[0].cost = 0.0;

        loop {
            let mut process_node: Option<usize> = None;

            for node in &nodes {
                // 訪問済み or まだコストが未設定
                if node.done || node.cost < 0.0 {
                    continue;
                }

                match process_node {
                    Option::None => process_node = Some(node.id),
                    Option::Some(pn) => {
                        if node.cost < nodes[pn].cost {
                            process_node = Some(node.id);
                        }
                    }
                }
            }

            match process_node {
                Option::Some(pn) => {
                    nodes[pn].done = true;

                    for edge in nodes[pn].edges.clone() {
                        let node = edge.0;
                        let cost = nodes[pn].cost + edge.1;

                        // コストが未設定 or コストの少ない経路がある場合はアップデート
                        let needs_update = (nodes[node].cost < 0.0) || (nodes[node].cost > cost);
                        if needs_update {
                            nodes[node].cost = cost;
                            nodes[node].previous_node = Some(pn);
                        }
                    }
                }
                None => {
                    break;
                }
            }
        }

        let goal_node = nodes.last().unwrap();

        let mut path: Vec<Node> = Vec::new();
        let mut current_node = goal_node;
        loop {
            let next_node = current_node.previous_node;
            match next_node {
                Option::None => {
                    break;
                }
                Option::Some(next_node) => {
                    path.push(nodes[next_node].clone());
                    current_node = &nodes[next_node];
                }
            }
        }

        let sum = path.clone()
            .into_iter()
            .filter_map(|x| {
                x.previous_node
                    .map(|id| x.edges.iter().find(|x| x.0 == id).unwrap().1)
            })
            .sum::<f64>() + path[0].edges.last().unwrap().1;

        (sum, path)
    }
}
