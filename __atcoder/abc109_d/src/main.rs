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
    input!(input=>(h:usize w:usize){h;list:[i64]});
    let mut res = Vec::new();
    let mut list = list;
    for i in 0..h {
        for j in 0..w {
            if j != w - 1 {
                //奇数なら右側に移動
                if list[i][j] % 2 != 0 {
                    list[i][j] -= 1;
                    list[i][j + 1] += 1;
                    res.push((i, j, i, j + 1));
                }
            } else {
                if i != h - 1 {
                    //奇数なら下に移動
                    if list[i][j] % 2 != 0 {
                        list[i][j] -= 1;
                        list[i + 1][j] += 1;
                        res.push((i, j, i + 1, j));
                    }
                }
            }
        }
    }
    let n = res.len();
    let res = res
        .into_iter()
        .map(|(a, b, c, d)| format!("{} {} {} {}\n", a + 1, b + 1, c + 1, d + 1))
        .collect::<String>();
    format!("{}\n{}", n, res).to_string()
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
    test1: "2 3\n1 2 3\n0 1 1" => "3\n2 2 2 3\n1 1 1 2\n1 3 1 2",
    test2: "3 2\n1 0\n2 1\n1 0" => "3\n1 1 1 2\n1 2 2 2\n3 1 3 2",
    test3: "1 5\n9 9 9 9 9" => "2\n1 1 1 2\n1 3 1 4",
}
