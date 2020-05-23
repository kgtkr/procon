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
    input!(input=>(n:usize)(q:usize){q;qs:[usize]});
    let mut is_flip = false;

    // 各行各列の現在の値は元の行列のどこに相当するか、ただしflipを考慮しない
    let mut row = (0..n).collect::<Vec<_>>();
    let mut col = (0..n).collect::<Vec<_>>();

    let mut res = Vec::new();
    for q in qs {
        match &q[..] {
            &[1, a, b] => {
                let a = a - 1;
                let b = b - 1;

                if !is_flip {
                    let x = row[a];
                    let y = row[b];

                    row[a] = y;
                    row[b] = x;
                } else {
                    let x = col[a];
                    let y = col[b];

                    col[a] = y;
                    col[b] = x;
                }
            }
            &[2, a, b] => {
                let a = a - 1;
                let b = b - 1;

                if !is_flip {
                    let x = col[a];
                    let y = col[b];

                    col[a] = y;
                    col[b] = x;
                } else {
                    let x = row[a];
                    let y = row[b];

                    row[a] = y;
                    row[b] = x;
                }
            }
            &[3] => {
                is_flip = !is_flip;
            }
            &[4, a, b] => {
                let a = a - 1;
                let b = b - 1;

                res.push(if !is_flip {
                    get_value(n, row[a], col[b])
                } else {
                    get_value(n, col[b], row[a])
                })
            }
            _ => panic!(),
        }
    }

    res.into_iter()
        .map(|x| x.to_string())
        .collect::<Vec<_>>()
        .join("\n")
}

fn get_value(n: usize, i: usize, j: usize) -> usize {
    n * i + j
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
    test1: "2\n19\n4 1 1\n4 1 2\n4 2 1\n4 2 2\n3\n4 1 1\n4 1 2\n4 2 1\n4 2 2\n1 1 2\n4 1 1\n4 1 2\n4 2 1\n4 2 2\n2 2 1\n4 1 1\n4 1 2\n4 2 1\n4 2 2\n" => "0\n1\n2\n3\n0\n2\n1\n3\n1\n3\n0\n2\n3\n1\n2\n0\n",
    test2: "3\n9\n2 2 3\n3\n1 2 1\n2 3 2\n1 1 3\n3\n4 1 1\n4 2 2\n4 2 3\n" => "1\n6\n8\n",
}
