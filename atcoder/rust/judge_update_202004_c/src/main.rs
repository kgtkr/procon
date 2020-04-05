extern crate core;

use std::collections::HashSet;
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
    input!(input=>(tup:(usize,usize,usize)));
    let (a1, a2, a3) = tup;
    let n = a1 + a2 + a3;
    let set = (1..=n).map(|x| x as i64).collect::<HashSet<_>>();
    let list = vec![
        std::iter::repeat(0).take(a1).collect::<Vec<_>>(),
        std::iter::repeat(0).take(a2).collect::<Vec<_>>(),
        std::iter::repeat(0).take(a3).collect::<Vec<_>>(),
    ];
    f(set, 0, 0, list).to_string()
}

fn f(set: HashSet<i64>, x: usize, y: usize, list: Vec<Vec<i64>>) -> i64 {
    if x == list.len() {
        1
    } else if y == list[x].len() {
        f(set, x + 1, 0, list)
    } else {
        let mut result = 0;
        for n in set.clone() {
            if (x == 0 || list[x - 1][y] < n) && (y == 0 || list[x][y - 1] < n) {
                let mut new_set = set.clone();
                new_set.remove(&n);
                let mut new_list = list.clone();
                new_list[x][y] = n;
                result += f(new_set, x, y + 1, new_list);
            }
        }
        result
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
    test1: "1 1 1\n" => "1\n",
    test2: "2 1 1\n" => "3\n",
    test3: "2 2 1\n" => "5\n",
}
