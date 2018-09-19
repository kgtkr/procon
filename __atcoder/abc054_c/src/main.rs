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
    input!(input=>(n:usize m:usize){m;list:(usize,usize)});
    let list = list
        .into_iter()
        .map(|(x, y)| (x - 1, y - 1))
        .collect::<Vec<_>>();

    let mut map = Vec::with_capacity(n);
    map.resize(n, {
        let mut vec = Vec::with_capacity(n);
        vec.resize(n, false);
        vec
    });

    for (a, b) in list {
        map[a][b] = true;
        map[b][a] = true;
    }

    let mut flag = Vec::with_capacity(n);
    flag.resize(n, false);
    flag[0] = true;

    f(n, &map, 1, 0, flag).to_string()
}

fn f(n: usize, list: &Vec<Vec<bool>>, road_len: usize, road_last: usize, flag: Vec<bool>) -> i64 {
    if road_len == n {
        1
    } else {
        let mut sum = 0;
        for i in 1..n {
            if !flag[i] {
                if list[road_last][i] {
                    sum += f(n, list, road_len + 1, i, {
                        let mut flag = flag.clone();
                        flag[i] = true;
                        flag
                    });
                }
            }
        }
        sum
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
    test1: "3 3\n1 2\n1 3\n2 3" => "2",
    test2: "7 7\n1 3\n2 7\n3 4\n4 5\n4 6\n5 6\n6 7" => "1",
}
