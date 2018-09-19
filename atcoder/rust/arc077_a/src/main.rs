extern crate core;

use std::io::{self, Read};

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = run(input.trim().to_string());
    println!("{}", output);
}

fn run(input: String) -> String {
    let list = input.split_whitespace().skip(1).collect::<Vec<_>>();
    let mut a = Vec::with_capacity(list.len() / 2);
    let mut b = Vec::with_capacity(list.len() / 2);
    let mut top = list.len() % 2 != 0;
    for &x in &list {
        if top {
            a.push(x);
        } else {
            b.push(x);
        }
        top = !top;
    }
    a.reverse();
    a.append(&mut b);
    a.join(" ").to_string()
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
    test1: "4
1 2 3 4" => "4 2 1 3",
    test2: "3
1 2 3" => "3 1 2",
    test3: "1
1000000000" => "1000000000",
    test4: "6
0 6 7 6 7 0" => "0 6 6 0 7 7",
}
