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
    input!(input=>(n:usize m:usize)(list:[i64]));
    // にぶたんでは？
    // 必ず前の人のほうが大きい。これは前後ろを逆順にする
    let mut eats = Vec::with_capacity(n);
    eats.resize(n, 0);
    let mut res = Vec::new();

    // 0 0 0 1 2

    for x in list {
        // 食う可能性がある人の数
        let less = less_count(&eats, &x);

        if less != 0 {
            res.push(Some(n - less));
            eats[less - 1] = x;
        } else {
            res.push(None);
        }
    }

    res.into_iter()
        .map(|x| x.map(|x| x as i64 + 1).unwrap_or(-1).to_string())
        .collect::<Vec<_>>()
        .join("\n")
}

//(小さい,同じ,大きい)
pub fn bound_count<T: Ord>(vec: &Vec<T>, val: &T) -> (usize, usize, usize) {
    let lb = lower_bound(vec, 0, vec.len(), val);
    let ub = upper_bound(vec, 0, vec.len(), val);
    (lb, ub - lb, vec.len() - ub)
}

//より小さい
pub fn less_count<T: Ord>(vec: &Vec<T>, val: &T) -> usize {
    lower_bound(vec, 0, vec.len(), val)
}

//より大きい
pub fn larger_count<T: Ord>(vec: &Vec<T>, val: &T) -> usize {
    vec.len() - upper_bound(vec, 0, vec.len(), val)
}

//同じ
pub fn eq_count<T: Ord>(vec: &Vec<T>, val: &T) -> usize {
    upper_bound(vec, 0, vec.len(), val) - lower_bound(vec, 0, vec.len(), val)
}

//vecは昇順ソート済み
//以上
pub fn lower_bound<T: Ord>(vec: &Vec<T>, mut first: usize, mut last: usize, val: &T) -> usize {
    let mut mid;
    while last - first > 1 {
        mid = (first + last) / 2;
        if &vec[mid] < val {
            first = mid;
        } else {
            last = mid;
        }
    }
    if &vec[first] < val {
        last
    } else {
        first
    }
}

//より大きい
pub fn upper_bound<T: Ord>(vec: &Vec<T>, mut first: usize, mut last: usize, val: &T) -> usize {
    let mut mid;
    while last - first > 1 {
        mid = (first + last) / 2;
        if &vec[mid] <= val {
            first = mid;
        } else {
            last = mid;
        }
    }
    if &vec[first] <= val {
        last
    } else {
        first
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
    test1: "2 5\n5 3 2 4 8\n" => "1\n2\n-1\n2\n1\n",
    test2: "5 10\n13 16 6 15 10 18 13 17 11 3\n" => "1\n1\n2\n2\n3\n1\n3\n2\n4\n5\n",
    test3: "10 30\n35 23 43 33 38 25 22 39 22 6 41 1 15 41 3 20 50 17 25 14 1 22 5 10 34 38 1 12 15 1\n" => "1\n2\n1\n2\n2\n3\n4\n2\n5\n6\n2\n7\n6\n3\n7\n6\n1\n7\n4\n8\n9\n6\n9\n9\n4\n4\n10\n9\n8\n-1\n",
}
