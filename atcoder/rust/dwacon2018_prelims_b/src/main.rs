extern crate core;

use std::collections::HashSet;
use std::io::{self, Read};

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = run(input.trim().to_string());
    println!("{}", output);
}

//切り出せたならsome
fn nico_remove(vec: &Vec<bool>) -> Option<Vec<bool>> {
    if vec.len() == 0 {
        None
    } else {
        let mut indexs: HashSet<usize> = HashSet::new();
        let mut find_bool = true;
        loop {
            let data = vec.iter()
                .enumerate()
                .skip(match indexs.iter().max() {
                    Some(&max) => max + 1,
                    None => 0,
                })
                .find(|&(_, &x)| x == find_bool);
            match data {
                Some((i, _)) => {
                    find_bool = !find_bool;
                    indexs.insert(i);
                }
                None => {
                    return if indexs.len() == 0 || !find_bool {
                        None
                    } else {
                        Some(
                            vec.iter()
                                .enumerate()
                                .filter(|&(i, _)| !indexs.contains(&i))
                                .map(|(_, &x)| x)
                                .collect::<Vec<bool>>(),
                        )
                    }
                }
            }
        }
    }
}

fn run(input: String) -> String {
    let mut list = input
        .chars()
        .map(|x| if x == '2' { true } else { false })
        .collect::<Vec<_>>();



    if list.len() == 0 {
        return "-1".to_string();
    }

    //分割
    let mut count = 0;
    loop {
        if list.len() == 0 {
            return count.to_string();
        }
        let data = nico_remove(&list);
        match data {
            Option::Some(new_list) => {
                count += 1;
                list = new_list;
            }
            Option::None => {
                return "-1".to_string();
            }
        }
    }
}

#[test]
fn test() {
    let long = "25".repeat(1000);
    let tests = vec![
        ("225525", "2"),
        ("52", "-1"),
        ("2255252252222555552522255255", "5"),
        ("25252", "-1"),
        (&long[..], "1"),
    ];
    for (i, (input, output)) in tests.into_iter().enumerate() {
        println!("test:{}", i);
        assert_eq!(run(input.to_string()), output.to_string());
    }
}
