#![feature(no_panic_pow)]

extern crate rayon;
use rayon::prelude::*;

fn main() {
    let mut res = (1..20181224u64 + 1)
        .into_par_iter()
        .filter(|&n| n_filter(n))
        .collect::<Vec<_>>();
    res.sort();
    for n in res {
        println!("{}", n);
    }
}

fn pair_ab(c: u64, list: Vec<char>) -> bool {
    get_a(c, Vec::new(), list)
}

fn get_a(c: u64, cur: Vec<(u64, u64)>, list: Vec<char>) -> bool {
    (1..=list.len() - 1).any(|n| {
        split_first_n(list.clone(), n)
            .map(|(a, list)| get_b(c, a, cur.clone(), list))
            .unwrap_or(false)
    })
}

fn get_b(c: u64, a: u64, cur: Vec<(u64, u64)>, list: Vec<char>) -> bool {
    //枝
    (1..=list.len()).filter(|&n| n != list.len() - 1).any(|n| {
        split_first_n(list.clone(), n)
            .map(|(b, list)| {
                let mut cur = cur.clone();
                cur.push((a, b));
                if list.len() == 0 {
                    list_filter(cur, c)
                } else {
                    get_a(c, cur, list)
                }
            })
            .unwrap_or(false)
    })
}

fn split_n(list: Vec<char>, n: usize) -> (Vec<char>, Vec<char>) {
    (
        list.clone().into_iter().take(n).collect(),
        list.into_iter().skip(n).collect(),
    )
}
// 先頭nを数値として返し、残りをリストとして返す
fn split_first_n(list: Vec<char>, n: usize) -> Option<(u64, Vec<char>)> {
    let (n, list) = split_n(list, n);
    if n[0] != '0' {
        Some((
            n.into_iter().collect::<String>().parse::<u64>().unwrap(),
            list,
        ))
    } else {
        None
    }
}

// 後方nを数値として返し、残りをリストとして返す
fn split_last_n(list: Vec<char>, n: usize) -> Option<(Vec<char>, u64)> {
    let n = list.len() - n;
    let (list, n) = split_n(list, n);
    if n[0] != '0' {
        Some((
            list,
            n.into_iter().collect::<String>().parse::<u64>().unwrap(),
        ))
    } else {
        None
    }
}

fn n_filter(n: u64) -> bool {
    if digit_count(n) < 3 {
        false
    } else {
        //cの桁数
        let list = n.to_string().chars().collect::<Vec<_>>();
        (1..=list.len() - 2).any(|n| {
            split_last_n(list.clone(), n)
                .map(|(list, c)| pair_ab(c, list))
                .unwrap_or(false)
        })
    }
}

fn list_filter(ab: Vec<(u64, u64)>, c: u64) -> bool {
    let x = {
        let mut s = String::new();
        for &(a, b) in &ab {
            s.push_str(&a.to_string());
            s.push_str(&b.to_string());
        }
        s.push_str(&c.to_string());
        s.parse::<u64>().unwrap()
    };

    let y = ab
        .into_iter()
        .map(|(a, b)| a.checked_pow(b as u32))
        .chain(std::iter::once(Some(c)))
        .try_fold(1u64, |a, b| b.and_then(|b| a.checked_mul(b)));

    Some(x) == y
}

fn digit_count(n: u64) -> i32 {
    if n < 10 {
        1
    } else if n < 100 {
        2
    } else if n < 1000 {
        3
    } else if n < 10000 {
        4
    } else if n < 100000 {
        5
    } else if n < 1000000 {
        6
    } else if n < 10000000 {
        7
    } else if n < 100000000 {
        8
    } else if n < 1000000000 {
        9
    } else {
        panic!();
    }
}
