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
    input!(input=>(n:usize a:usize b:usize)(list:[i64]));
    let mut list = list;
    list.sort();
    list.reverse();
    let min_v = list.clone().into_iter().nth(a - 1).unwrap();
    let ex_v = list
        .clone()
        .into_iter()
        .take(a)
        .filter(|&x| x == min_v)
        .count();
    let count_v = list.clone().into_iter().filter(|&x| x == min_v).count();
    //count_v C ex_v
    let mut abs_max = (list.clone().into_iter().take(a).sum::<i64>() as f64 / a as f64);

    let all_eq = {
        let val = list[0];
        let mut res = true;
        for x in list.clone().into_iter().take(a) {
            if x != val {
                res = false;
                break;
            }
        }
        res
    };

    //ex_vからmin(b,count_v)選ぶ
    //ただし全て同じなら
    let mut pt = 0;
    if all_eq {
        for r in ex_v..(std::cmp::min(b, count_v) + 1) {
            pt += ncr(count_v as i64, r as i64);
        }
    } else {
        pt = ncr(count_v as i64, ex_v as i64);
    }

    format!("{}\n{}", abs_max, pt).to_string()
}

fn ncr(n: i64, r: i64) -> i64 {
    if r == 0 {
        1
    } else {
        (n - r + 1) * ncr(n, r - 1) / r
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
    test1: "5 2 2\n1 2 3 4 5" => "4.500000\n1",
    test2: "4 2 3\n10 20 10 10" => "15.000000\n3",
    test3: "5 1 5\n1000000000000000 999999999999999 999999999999998 999999999999997 999999999999996" => "1000000000000000.000000\n1",
    test4: "50 1 50\n1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1" => "1.000000\n1125899906842623",
}
