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
    input!(input=>(n:usize)(list:[i64]));
    ({
        let mut first = list.iter().min().cloned().unwrap();
        let mut last = list.iter().max().cloned().unwrap();
        while last - first > 1 {
            let mid = (first + last) / 2;
            if is_top_gte(&list, mid) {
                first = mid;
            } else {
                last = mid;
            }
        }
        if is_top_gte(&list, first) {
            first
        } else {
            last
        }
    })
    .to_string()
}

// listが一番下の段のピラミッドの一番上がxより大きくなるか
fn is_top_gte(list: &Vec<i64>, x: i64) -> bool {
    let c_i = list.len() / 2;

    // 左を探索
    let left = {
        let mut res = None;
        let mut prev = list[c_i] >= x;
        for (d, i) in (0..c_i).rev().enumerate() {
            let cur = list[i] >= x;

            if prev == cur {
                res = Some((d, cur));
                break;
            }
            prev = cur;
        }
        res
    };

    // 右を探索
    let right = {
        let mut res = None;
        let mut prev = list[c_i] >= x;
        for (d, i) in ((c_i + 1)..list.len()).enumerate() {
            let cur = list[i] >= x;

            if prev == cur {
                res = Some((d, cur));
                break;
            }
            prev = cur;
        }
        res
    };

    match (left, right) {
        (Some((ld, gte)), Some((rd, _))) if ld < rd => gte,
        (Some(_), Some((_, gte))) => gte,
        (None, Some((_, gte))) => gte,
        (Some((_, gte)), None) => gte,
        (None, None) => {
            if list.len() % 4 == 1 {
                list[c_i] > x
            } else {
                !(list[c_i] > x)
            }
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
    test1: "4\n1 6 3 7 4 5 2\n" => "4\n",
    test2: "2\n1 2 3\n" => "2\n",
}
