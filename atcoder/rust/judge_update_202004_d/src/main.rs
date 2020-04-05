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
    input!(input=>(n:usize q:usize)(a_list:[i64])(s_list:[i64]));
    let mut gcd_a = gcd_seq(a_list);
    gcd_a.reverse();
    s_list
        .into_iter()
        .map(|x| {
            let (lo, eq, up) = bound_count(&|&y| gcd(x, y), &gcd_a, &1);
            if eq != 0 {
                up as i64 + 1
            } else {
                gcd(x, gcd_a[0])
            }
        })
        .map(|x| x.to_string())
        .collect::<Vec<_>>()
        .join("\n")
}

pub fn gcd_seq(v: Vec<i64>) -> Vec<i64> {
    let init = v[0];
    v.into_iter()
        .scan(init, |state, x| {
            *state = gcd(*state, x);
            Some(*state)
        })
        .collect()
}

pub fn gcd(a: i64, b: i64) -> i64 {
    if b == 0 {
        a
    } else {
        gcd(b, a % b)
    }
}

//(小さい,同じ,大きい)
pub fn bound_count<T: Ord>(f: &impl Fn(&T) -> T, vec: &Vec<T>, val: &T) -> (usize, usize, usize) {
    let lb = lower_bound(f, vec, 0, vec.len(), val);
    let ub = upper_bound(f, vec, 0, vec.len(), val);
    (lb, ub - lb, vec.len() - ub)
}

//より小さい
pub fn less_count<T: Ord>(f: &impl Fn(&T) -> T, vec: &Vec<T>, val: &T) -> usize {
    lower_bound(f, vec, 0, vec.len(), val)
}

//より大きい
pub fn larger_count<T: Ord>(f: &impl Fn(&T) -> T, vec: &Vec<T>, val: &T) -> usize {
    vec.len() - upper_bound(f, vec, 0, vec.len(), val)
}

//同じ
pub fn eq_count<T: Ord>(f: &impl Fn(&T) -> T, vec: &Vec<T>, val: &T) -> usize {
    upper_bound(f, vec, 0, vec.len(), val) - lower_bound(f, vec, 0, vec.len(), val)
}

//vecは昇順ソート済み
//以上
pub fn lower_bound<T: Ord>(
    f: &impl Fn(&T) -> T,
    vec: &Vec<T>,
    mut first: usize,
    mut last: usize,
    val: &T,
) -> usize {
    let mut mid;
    while last - first > 1 {
        mid = (first + last) / 2;
        if &f(&vec[mid]) < val {
            first = mid;
        } else {
            last = mid;
        }
    }
    if &f(&vec[first]) < val {
        last
    } else {
        first
    }
}

//より大きい
pub fn upper_bound<T: Ord>(
    f: &impl Fn(&T) -> T,
    vec: &Vec<T>,
    mut first: usize,
    mut last: usize,
    val: &T,
) -> usize {
    let mut mid;
    while last - first > 1 {
        mid = (first + last) / 2;
        if &f(&vec[mid]) <= val {
            first = mid;
        } else {
            last = mid;
        }
    }
    if &f(&vec[first]) <= val {
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
    test1: "4 3\n6 12 6 9\n4 6 3\n" => "4\n3\n3\n",
    test2: "4 3\n4 6 2 1\n3 2 1000000000\n" => "1\n4\n4\n",
}
