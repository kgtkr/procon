extern crate core;

use std::io::{self, Read};
use std::cmp;

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = run(input.trim().to_string());
    println!("{}", output);
}

fn run(input: String) -> String {
    let list = input
        .split_whitespace()
        .skip(1)
        .map(|x| x.parse::<i64>().unwrap())
        .collect::<Vec<_>>();
    let mut free = 0;
    let mut a = 0;
    let mut b = 0;
    let mut c = 0;
    let mut d = 0;
    let mut e = 0;
    let mut f = 0;
    let mut g = 0;
    let mut h = 0;

    /*
    1-399：a 
    400-799：b
    800-1199：c
    1200-1599：d
    1600-1999：e
    2000-2399：f
    2400-2799：g
    2800-3199：h
    3200- free
     */

    for x in list {
        if x <= 399 {
            a = 1;
        } else if x <= 799 {
            b = 1;
        } else if x <= 1199 {
            c = 1;
        } else if x <= 1599 {
            d = 1;
        } else if x <= 1999 {
            e = 1;
        } else if x <= 2399 {
            f = 1;
        } else if x <= 2799 {
            g = 1;
        } else if x <= 3199 {
            h = 1;
        } else {
            free += 1;
        }
    }
    let count = a + b + c + d + e + f + g + h;
    let min = cmp::max(count, 1);
    let max = count + free;
    format!("{} {}", min, max).to_string()
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
2100 2500 2700 2700" => "2 2",
    test2: "5
1100 1900 2800 3200 3200" => "3 5",
    test3: "20
800 810 820 830 840 850 860 870 880 890 900 910 920 930 940 950 960 970 980 990" => "1 1",
}
