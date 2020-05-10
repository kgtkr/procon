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
    input!(input=>(n:usize m:usize x:i64){n;list: (i64,[i64])});
    f(
        x,
        0,
        0,
        {
            let mut v = Vec::with_capacity(m);
            v.resize(m, 0);
            v
        },
        &list,
    )
    .unwrap_or(-1)
    .to_string()
}

fn f(x: i64, i: usize, yen: i64, cur: Vec<i64>, list: &Vec<(i64, Vec<i64>)>) -> Option<i64> {
    if i == list.len() {
        if cur.iter().all(|&a| a >= x) {
            Some(yen)
        } else {
            None
        }
    } else {
        let a = f(x, i + 1, yen, cur.clone(), list);
        let b = f(
            x,
            i + 1,
            yen + list[i].0,
            cur.into_iter()
                .zip(&list[i].1)
                .map(|(x, y)| x + y)
                .collect::<Vec<_>>(),
            list,
        );

        match (a, b) {
            (Some(a), Some(b)) => Some(a.min(b)),
            (Some(a), None) => Some(a),
            (None, Some(b)) => Some(b),
            (None, None) => None,
        }
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
    test1: "3 3 10\n60 2 2 4\n70 8 7 9\n50 2 3 9\n" => "120\n",
    test2: "3 3 10\n100 3 1 4\n100 1 5 9\n100 2 6 5\n" => "-1\n",
    test3: "8 5 22\n100 3 7 5 3 1\n164 4 5 2 7 8\n334 7 2 7 2 9\n234 4 7 2 8 2\n541 5 4 3 3 6\n235 4 8 6 9 7\n394 3 6 1 6 2\n872 8 4 3 7 2\n" => "1067\n",
}
