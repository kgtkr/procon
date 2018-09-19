extern crate core;

use std::io::{self, Read};

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = run(input.trim().to_string());
    println!("{}", output);
}

fn run(input: String) -> String {
    let n = input
        .split_whitespace()
        .nth(0)
        .unwrap()
        .parse::<usize>()
        .unwrap();

    let goal = n - 1;

    let list = input
        .split("\n")
        .skip(1)
        .flat_map(|x| {
            let l = x.split_whitespace()
                .map(|x| x.parse::<usize>().unwrap() - 1)
                .collect::<Vec<_>>();
            vec![(l[0], l[1]), (l[1], l[0])]
        })
        .collect::<Vec<_>>();

    let mut nodes = Vec::new();
    for _ in 0..n {
        nodes.push(vec![]);
    }

    for (a, b) in list.into_iter() {
        nodes[a].push(b);
    }

    for &route in &nodes[0] {
        if route == goal {
            return "POSSIBLE".to_string();
        }

        for &route2 in &nodes[route] {
            if route2 == goal {
                return "POSSIBLE".to_string();
            }
        }
    }

    "IMPOSSIBLE".to_string()
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
    test1: "3 2
1 2
2 3" => "POSSIBLE",
    test2: "4 3
1 2
2 3
3 4" => "IMPOSSIBLE",
    test3: "100000 1
1 99999" => "IMPOSSIBLE",
    test4: "5 5
1 3
4 5
2 3
2 4
1 4" => "POSSIBLE",
}
