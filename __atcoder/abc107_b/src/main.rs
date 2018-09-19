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
    input!(input=>(h:usize w:usize){h;list:#});
    let list = list
        .into_iter()
        .map(|x| x.chars().map(|x| x == '#').collect::<Vec<_>>())
        .collect::<Vec<_>>();
    let r = reverse_2d(
        reverse_2d(
            list.into_iter()
                .filter(|x| x.iter().any(|&x| x))
                .collect::<Vec<_>>(),
        ).into_iter()
            .filter(|x| x.iter().any(|&x| x))
            .collect::<Vec<_>>(),
    ).into_iter()
        .map(|x| {
            x.into_iter()
                .map(|x| if x { '#' } else { '.' })
                .collect::<String>()
        })
        .collect::<Vec<_>>();
    r.join("\n")
}

fn reverse_2d<T: Clone + Default>(v: Vec<Vec<T>>) -> Vec<Vec<T>> {
    let h = v.len();
    if h == 0 {
        Vec::new()
    } else {
        let w = v[0].len();
        let mut res = Vec::with_capacity(w);
        res.resize(w, {
            let mut v = Vec::with_capacity(h);
            v.resize(h, T::default());
            v
        });
        for i in 0..h {
            for j in 0..w {
                res[j][i] = v[i][j].clone();
            }
        }
        res
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
    test1: "4 4\n##.#\n....\n##.#\n.#.#" => "###\n###\n.##",
    test2: "3 3\n#..\n.#.\n..#" => "#..\n.#.\n..#",
    test3: "4 5\n.....\n.....\n..#..\n....." => "#",
    test4: "7 6\n......\n....#.\n.#....\n..#...\n..#...\n......\n.#..#." => "..#\n#..\n.#.\n.#.\n#.#",
}
