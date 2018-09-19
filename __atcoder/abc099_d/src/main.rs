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
    input!(input=>(n:usize c:usize){c;d_list:[i64]}{n;c_list:[usize]});
    //1列目が色iの時の違和感の合計
    let d1 = (0..c)
        .map(|c| {
            c_list
                .clone()
                .into_iter()
                .map(|x| x.into_iter().map(|x| d_list[x - 1][c]).collect::<Vec<_>>())
                .collect::<Vec<_>>()
        })
        .map(|list| {
            let mut sum_0 = 0;
            let mut sum_1 = 0;
            let mut sum_2 = 0;
            for i in 0..n {
                for j in 0..n {
                    match (i + j + 2) % 3 {
                        0 => {
                            sum_0 += list[i][j];
                        }
                        1 => {
                            sum_1 += list[i][j];
                        }
                        2 => {
                            sum_2 += list[i][j];
                        }
                        _ => {}
                    }
                }
            }
            (sum_0, sum_1, sum_2)
        })
        .collect::<Vec<_>>();

    let mut min = 10000000000;
    for a in 0..c {
        for b in 0..c {
            for c in 0..c {
                if a != b && a != c && b != c {
                    let x = d1[a].0 + d1[b].1 + d1[c].2;
                    if x < min {
                        min = x;
                    }
                }
            }
        }
    }
    min.to_string()
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
    test1: "2 3\n0 1 1\n1 0 1\n1 4 0\n1 2\n3 3" => "3",
    test2: "4 3\n0 12 71\n81 0 53\n14 92 0\n1 1 2 1\n2 1 1 2\n2 2 1 3\n1 1 2 2" => "428",
}
