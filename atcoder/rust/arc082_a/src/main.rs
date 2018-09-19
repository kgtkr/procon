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
    input
        .split_whitespace()
        .skip(1)
        .map(|x| x.parse::<i32>().unwrap())
        .flat_map(|x| vec![x, x + 1, x - 1])
        .fold(HashMap::new(), |mut map, x| {
            let v = map.get(&x).map(|i| i + 1).unwrap_or(1);
            map.insert(x, v);
            map
        })
        .into_iter()
        .fold(None, |a, (x, y)| match a {
            Option::None => Some((x, y)),
            Option::Some((_, q)) if y > q => Some((x, y)),
            _ => a,
        })
        .unwrap()
        .1
        .to_string()
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
    test1: "7
3 1 4 1 5 9 2" => "4",
    test2: "10
0 1 2 3 4 5 6 7 8 9" => "3",
test3:"1
99999"=>"1",
}
