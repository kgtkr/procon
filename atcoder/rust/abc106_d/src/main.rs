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
    input!(input=>(n:usize m:usize q:usize){m;list:(@,@)}{q;query:(@,@)});

    let mut map = Vec::with_capacity(n);
    map.resize(n, {
        let mut v = Vec::with_capacity(n);
        v.resize(n, 0i64);
        v
    });

    for (x, y) in list {
        map[x][y] += 1;
    }

    let map_sum = sum_seq_2d(map);

    let mut res = Vec::new();
    for (l, r) in query {
        res.push(sum_seq_2d_rect(&map_sum, (l, l), (r, r)))
    }

    res.into_iter()
        .map(|x| x.to_string())
        .collect::<Vec<_>>()
        .join("\n")
        .to_string()
}

pub fn sum_seq_2d(mut vec: Vec<Vec<i64>>) -> Vec<Vec<i64>> {
    for i in 0..vec.len() {
        for j in 0..vec[0].len() {
            vec[i][j] += if j != 0 { vec[i][j - 1] } else { 0 };
        }
    }

    for i in 0..vec.len() {
        for j in 0..vec[0].len() {
            vec[i][j] += if i != 0 { vec[i - 1][j] } else { 0 };
        }
    }

    vec
}

pub fn sum_seq_2d_rect(
    sum_vec: &Vec<Vec<i64>>,
    (x1, y1): (usize, usize),
    (x2, y2): (usize, usize),
) -> i64 {
    sum_vec[x2][y2] + (if x1 != 0 && y1 != 0 {
        sum_vec[x1 - 1][y1 - 1]
    } else {
        0
    }) - (if x1 != 0 { sum_vec[x1 - 1][y2] } else { 0 }) - (if y1 != 0 {
        sum_vec[x2][y1 - 1]
    } else {
        0
    })
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
    test1: "2 3 1\n1 1\n1 2\n2 2\n1 2" => "3",
    test2: "10 3 2\n1 5\n2 8\n7 10\n1 7\n3 10" => "1\n1",
    test3: "10 10 10\n1 6\n2 9\n4 5\n4 7\n4 7\n5 8\n6 6\n6 7\n7 9\n10 10\n1 8\n1 9\n1 10\n2 8\n2 9\n2 10\n3 8\n3 9\n3 10\n1 10" => "7\n9\n10\n6\n8\n9\n6\n7\n8\n10",
}
