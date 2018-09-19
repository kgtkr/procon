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
    input!(input=>(h:usize w:usize){h;s_list:#});
    let map = s_list
        .into_iter()
        .map(|s| {
            s.chars()
                .map(|x| if x == '#' { 1 } else { 0 })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    let mut res = {
        let mut v = Vec::with_capacity(h);
        v.resize(h, {
            let mut v = Vec::with_capacity(w);
            v.resize(w, None);
            v
        });
        v
    };

    for y in 0..h {
        for x in 0..w {
            if map[y][x] == 0 {
                res[y][x] = Some(
                    if y != 0 {
                        (if x != 0 { map[y - 1][x - 1] } else { 0 })
                            + map[y - 1][x]
                            + if x != w - 1 { map[y - 1][x + 1] } else { 0 }
                    } else {
                        0
                    } + {
                        (if x != 0 { map[y][x - 1] } else { 0 })
                            + if x != w - 1 { map[y][x + 1] } else { 0 }
                    } + if y != h - 1 {
                        (if x != 0 { map[y + 1][x - 1] } else { 0 })
                            + map[y + 1][x]
                            + if x != w - 1 { map[y + 1][x + 1] } else { 0 }
                    } else {
                        0
                    },
                );
            }
        }
    }

    res.into_iter()
        .map(|x| {
            x.into_iter()
                .map(|x| x.map(|x| x.to_string()).unwrap_or("#".to_string()))
                .collect::<Vec<_>>()
                .join("")
        })
        .collect::<Vec<_>>()
        .join("\n")
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
    test1: "3 5\n.....\n.#.#.\n....." => "11211\n1#2#1\n11211",
    test2: "3 5\n#####\n#####\n#####" => "#####\n#####\n#####",
    test3: "6 6\n#####.\n#.#.##\n####.#\n.#..#.\n#.##..\n#.#..." => "#####3\n#8#7##\n####5#\n4#65#2\n#5##21\n#4#310",
}
