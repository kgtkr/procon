extern crate core;

use std::io::{self, Read};

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = run(input.trim().to_string());
    println!("{}", output);
}

fn run(input: String) -> String {
    let list = input.split("\n").skip(1).map(|line| {
        let ll = line.split_whitespace()
            .map(|x| x.parse::<usize>().unwrap())
            .collect::<Vec<_>>();
        //l,r
        (ll[0], ll[1])
    });

    let primes = generate_prime(100000);
    let mut is_prime = Vec::with_capacity(100001);
    for prime in primes {
        for _ in 0..prime - is_prime.len() {
            is_prime.push(false);
        }
        is_prime.push(true);
    }

    for _ in 0..100 {
        is_prime.push(false);
    }

    //似た数の個数の累積和
    let count = (0..100001)
        .map(|n| {
            if is_prime[n] && is_prime[(n + 1) / 2] {
                1
            } else {
                0
            }
        })
        .scan(0, |state, x| {
            *state = *state + x;
            Some(*state)
        })
        .collect::<Vec<i32>>();

    list.map(|(l, r)| (count[r] - count[l - 1]).to_string())
        .collect::<Vec<_>>()
        .join("\n")
}

fn generate_prime(max: usize) -> Vec<usize> {
    let sqrt_max = (max as f64).sqrt();
    let mut prime_list = Vec::new();

    let mut search_list = (2..max + 1).collect::<Vec<_>>();

    while {
        let prime = search_list[0];
        prime_list.push(prime);
        search_list = search_list.into_iter().filter(|n| n % prime != 0).collect();

        (prime as f64) < sqrt_max
    } {}

    prime_list.append(&mut search_list);

    prime_list
}

#[test]
fn test() {
    let tests = vec![
        ("1\n3 7", "2"),
        ("4\n13 13\n7 11\n7 11\n2017 2017", "1\n0\n0\n1"),
        (
            "6\n1 53\n13 91\n37 55\n19 51\n73 91\n13 49",
            "4\n4\n1\n1\n1\n2",
        ),
    ];
    for (i, (input, output)) in tests.into_iter().enumerate() {
        println!("test:{}", i);
        assert_eq!(run(input.to_string()), output.to_string());
    }
}
