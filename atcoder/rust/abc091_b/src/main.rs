extern crate core;

use std::io::{self, Read};
use std::collections::HashMap;

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = run(input.trim().to_string());
    println!("{}", output);
}

fn run(input: String) -> String {
    let list = input.split_whitespace().collect::<Vec<_>>();
    let n = list[0].parse::<usize>().unwrap();
    let s = list.clone().into_iter().skip(1).take(n).collect::<Vec<_>>();
    let m = list[1 + n].parse::<usize>().unwrap();
    let t = list.clone()
        .into_iter()
        .skip(2 + n)
        .take(m)
        .collect::<Vec<_>>();
    let mut map = HashMap::new();
    for x in s {
        let v = map.get(&x).map(|x| x + 1).unwrap_or(1);
        map.insert(x, v);
    }

    for x in t {
        let v = map.get(&x).map(|x| x - 1).unwrap_or(-1);
        map.insert(x, v);
    }

    let mut max = None;
    for (a, b) in map.into_iter() {
        max = Some(match max {
            None => (a, b),
            Some((_, y)) if b > y => (a, b),
            Some((x, y)) => (x, y),
        });
    }

    std::cmp::max(max.unwrap().1, 0).to_string()
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
    test1: "3
apple
orange
apple
1
grape" => "2",
    test2: "3
apple
orange
apple
5
apple
apple
apple
apple
apple" => "1",
    test3: "1
voldemort
10
voldemort
voldemort
voldemort
voldemort
voldemort
voldemort
voldemort
voldemort
voldemort
voldemort" => "0",
    test4: "6
red
red
blue
yellow
yellow
red
5
red
red
yellow
green
blue" => "1",
}
