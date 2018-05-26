extern crate core;

use std::io::{self, Read};
use std::collections::HashMap;

#[macro_use]
mod parser {
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
    ($line:expr,$name:ident,$t:tt) => {
        let $name=value!($line,$t);
    };
    }

    macro_rules! values_def {
    ($lines:expr,$n:expr,$name:ident,$t:tt) => {
        let $name={
        let mut vec=Vec::new();
        for i in 0..$n{
            let mut next=$lines.next().unwrap().split_whitespace();
            vec.push(value!(next,$t));
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
    //単一値
    ($line:expr,$t:ty) => {
        $line.next().unwrap().parse::<$t>().unwrap()
    };
    }
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = solve(input.trim().to_string());
    println!("{}", output);
}

fn solve(input: String) -> String {
    input!(input=>(n:usize)(list:[i64]));
    let sum = list.clone()
        .into_iter()
        .scan(0, |state, x| {
            *state = *state + x;
            Some(*state)
        })
        .collect::<Vec<_>>();

    println!("sum:{:?}", sum);

    let xor = list.clone()
        .into_iter()
        .scan(0, |state, x| {
            *state = *state ^ x;
            Some(*state)
        })
        .enumerate()
        .collect::<Vec<_>>();

    println!("xor:{:?}", xor);

    let mut map = HashMap::new();
    for (i, x) in xor {
        let ent = map.entry(x).or_insert(vec![]);
        ent.push(i);
    }

    println!("xor_map:{:?}", map);

    let mut count = 0;
    for (i, x) in sum.into_iter().enumerate() {
        match map.get(&x) {
            Some(bts) => {
                let (a, b, c) = bound::bound_count(&bts, &i);
                count += a + b;
            }
            _ => {}
        }
    }

    count.to_string()
}

mod bound {
    //参考 http://www.pandanoir.info/entry/2015/12/26/190000

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

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn test1() {
            let vec = vec![-1, 0, 0, 0, 1, 2, 2, 5, 5, 9];
            let len = vec.len();

            assert_eq!(0, lower_bound(&vec, 0, len, &-2));
            assert_eq!(0, upper_bound(&vec, 0, len, &-2));
            assert_eq!(0, less_count(&vec, &-2));
            assert_eq!(0, eq_count(&vec, &-2));
            assert_eq!(10, larger_count(&vec, &-2));
            assert_eq!((0, 0, 10), bound_count(&vec, &-2));

            assert_eq!(0, lower_bound(&vec, 0, len, &-1));
            assert_eq!(1, upper_bound(&vec, 0, len, &-1));
            assert_eq!(0, less_count(&vec, &-1));
            assert_eq!(1, eq_count(&vec, &-1));
            assert_eq!(9, larger_count(&vec, &-1));
            assert_eq!((0, 1, 9), bound_count(&vec, &-1));

            assert_eq!(1, lower_bound(&vec, 0, len, &0));
            assert_eq!(4, upper_bound(&vec, 0, len, &0));
            assert_eq!(1, less_count(&vec, &0));
            assert_eq!(3, eq_count(&vec, &0));
            assert_eq!(6, larger_count(&vec, &0));
            assert_eq!((1, 3, 6), bound_count(&vec, &0));

            assert_eq!(9, lower_bound(&vec, 0, len, &9));
            assert_eq!(10, upper_bound(&vec, 0, len, &9));
            assert_eq!(9, less_count(&vec, &9));
            assert_eq!(1, eq_count(&vec, &9));
            assert_eq!(0, larger_count(&vec, &9));
            assert_eq!((9, 1, 0), bound_count(&vec, &9));

            assert_eq!(10, lower_bound(&vec, 0, len, &10));
            assert_eq!(10, upper_bound(&vec, 0, len, &10));
            assert_eq!(10, less_count(&vec, &10));
            assert_eq!(0, eq_count(&vec, &10));
            assert_eq!(0, larger_count(&vec, &10));
            assert_eq!((10, 0, 0), bound_count(&vec, &10));
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
    test1: "4\n2 5 4 6" => "5",
    test2: "9\n0 0 0 0 0 0 0 0 0" => "45",
    test3: "19\n885 8 1 128 83 32 256 206 639 16 4 128 689 32 8 64 885 969 1" => "37",
}
