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
    ($line:expr,$name:ident,$t:tt) => {
        let $name=value!($line,$t);
    };
    }

    macro_rules! values_def {
    ($lines:expr,$n:expr,$name:ident,$t:tt) => {
        let $name={
        let mut vec=Vec::new();
        for i in 0..$n{
            let mut next=$lines.next().unwrap().split_whitespace();
            vec.push(value!(next,$t));
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
    let output = run(input.trim().to_string());
    println!("{}", output);
}

fn run(input: String) -> String {
    input!(input=>(a:i64 b:i64));
    //白かどうか
    let mut w_map = [[false; 100]; 50];
    {
        let mut w_count = 1;
        for i in (1..49).filter(|x| x % 2 != 0) {
            for j in (1..99).filter(|x| x % 2 != 0) {
                if w_count < a {
                    w_map[i][j] = true;
                    w_count += 1;
                }
            }
        }
    }

    //黒かどうか
    let mut b_map = [[false; 100]; 50];
    {
        let mut b_count = 1;
        for i in (1..49).filter(|x| x % 2 != 0) {
            for j in (1..99).filter(|x| x % 2 != 0) {
                if b_count < b {
                    b_map[i][j] = true;
                    b_count += 1;
                }
            }
        }
    }

    format!(
        "100 100\n{}\n{}",
        w_map
            .into_iter()
            .map(|x| x.into_iter()
                .map(|&x| if x { "." } else { "#" })
                .collect::<Vec<_>>()
                .join(""))
            .collect::<Vec<_>>()
            .join("\n"),
        b_map
            .into_iter()
            .map(|x| x.into_iter()
                .map(|&x| if x { "#" } else { "." })
                .collect::<Vec<_>>()
                .join(""))
            .collect::<Vec<_>>()
            .join("\n")
    ).to_string()
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
    test1: "2 3" => "3 3\n##.\n..#\n#.#",
    test2: "7 8" => "3 5\n#.#.#\n.#.#.\n#.#.#",
    test3: "1 1" => "4 2\n..\n#.\n##\n##",
    test4: "3 14" => "8 18\n..................\n..................\n....##.......####.\n....#.#.....#.....\n...#...#....#.....\n..#.###.#...#.....\n.#.......#..#.....\n#.........#..####.",
}
