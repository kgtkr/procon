extern crate core;

use std::io::{self, Read};

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = run(input.trim().to_string());
    println!("{}", output);
}

fn run(input: String) -> String {
    let mut iter = input.split("\n");

    let (n, m) = {
        let l = iter.next()
            .unwrap()
            .split_whitespace()
            .map(|x| x.parse::<usize>().unwrap())
            .collect::<Vec<_>>();
        (l[0], l[1])
    };

    let list = iter.map(|x| {
        let l = x.split_whitespace()
            .map(|x| x.parse::<usize>().unwrap())
            .collect::<Vec<_>>();
        (l[0] - 1, l[1] - 1)
    }).collect::<Vec<_>>();

    let mut count = 0;
    //一個ずつ外していく
    for i in 0..list.len() {
        let list = {
            //辺削除
            let mut list = list.clone();
            list.remove(i);

            //逆向きの辺を貼る
            list.into_iter()
                .flat_map(|(a, b)| vec![(a, b), (b, a)])
                .collect::<Vec<_>>()
        };

        //検索しやすいような形にする
        let mut nodes = Vec::new();
        for _ in 0..n {
            nodes.push((false, Vec::new()));
        }
        for (a, b) in list {
            nodes[a].1.push(b);
        }

        //連結判定
        fn f(nodes: &mut Vec<(bool, Vec<usize>)>, now: usize) {
            if nodes[now].0 {
                return;
            }
            nodes[now].0 = true;
            for x in nodes[now].1.clone() {
                f(nodes, x);
            }
        }

        f(&mut nodes, 0);

        if !nodes.into_iter().all(|x| x.0) {
            count += 1;
        }
    }

    count.to_string()
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
    test1: "7 7
1 3
2 7
3 4
4 5
4 6
5 6
6 7" => "4",
    test2: "3 3
1 2
1 3
2 3" => "0",
test3:"6 5
1 2
2 3
3 4
4 5
5 6"=>"5",
}
