extern crate core;

use std::io::{self, Read};

#[macro_export]
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
  //インデックス(-1)
  ($line:expr,@) => {
    $line.next().unwrap().parse::<usize>().unwrap()-1
  };
  //単一値
  ($line:expr,$t:ty) => {
    $line.next().unwrap().parse::<$t>().unwrap()
  };
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = solve(input.trim().to_string());
    println!("{}", output);
}

fn solve(input: String) -> String {
    input!(input=>(n:usize){5;list:#});
    let list = list
        .into_iter()
        .map(|s| s.chars().map(|c| c == '#').collect::<Vec<_>>())
        .collect::<Vec<_>>();

    let pats =  ".###..#..###.###.#.#.###.###.###.###.###.\n.#.#.##....#...#.#.#.#...#.....#.#.#.#.#.\n.#.#..#..###.###.###.###.###...#.###.###.\n.#.#..#..#.....#...#...#.#.#...#.#.#...#.\n.###.###.###.###...#.###.###...#.###.###."
        .split("\n")
        .map(|s| s.chars().map(|c| c == '#').collect::<Vec<_>>())
        .collect::<Vec<_>>();

    let mut res = Vec::new();
    for i in 0..n {
        res.push((0..10).find(|&j| is_match(&list, &pats, i, j)).unwrap());
    }

    res.into_iter()
        .map(|x| x.to_string())
        .collect::<Vec<_>>()
        .join("")
        .to_string()
}

// aのx番目とbのy番目が一致しているか
fn is_match(a: &Vec<Vec<bool>>, b: &Vec<Vec<bool>>, x: usize, y: usize) -> bool {
    // a、bどれだけ左から空けるか
    let x_skip = 4 * x + 1;
    let y_skip = 4 * y + 1;

    for i in 0..5 {
        for j in 0..3 {
            if a[i][x_skip + j] != b[i][y_skip + j] {
                return false;
            }
        }
    }

    return true;
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
    test1: "10\n.###..#..###.###.#.#.###.###.###.###.###.\n.#.#.##....#...#.#.#.#...#.....#.#.#.#.#.\n.#.#..#..###.###.###.###.###...#.###.###.\n.#.#..#..#.....#...#...#.#.#...#.#.#...#.\n.###.###.###.###...#.###.###...#.###.###.\n" => "0123456789\n",
    test2: "29\n.###.###.###.###.###.###.###.###.###.#.#.###.#.#.#.#.#.#.###.###.###.###..#..###.###.###.###.###.#.#.###.###.###.###.\n...#.#.#...#.#.#.#.#.#...#.#...#.#.#.#.#.#...#.#.#.#.#.#.#.....#.#.#.#.#.##..#.#...#.#.#...#.#...#.#...#.#.....#...#.\n.###.#.#...#.###.#.#.###.###...#.###.###.###.###.###.###.###...#.###.#.#..#..###...#.###.###.###.###.###.###.###.###.\n.#...#.#...#...#.#.#.#.#...#...#.#.#...#.#.#...#...#...#.#.#...#...#.#.#..#..#.#...#...#.#...#.#...#.#.....#...#.#...\n.###.###...#.###.###.###.###...#.###...#.###...#...#...#.###...#.###.###.###.###...#.###.###.###...#.###.###.###.###.\n" => "20790697846444679018792642532\n",
}
