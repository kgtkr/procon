extern crate core;

use std::io::{self, Read};

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = run(input.trim().to_string());
    println!("{}", output);
}

fn run(input: String) -> String {
    let list = input
        .chars()
        .map(|x| x.to_string().parse::<i32>().unwrap())
        .collect::<Vec<_>>();
    // 総当たり
    for i in 0..1 << 4 {
        let ap = i & 1 << 3 == 0;
        let bp = i & 1 << 2 == 0;
        let cp = i & 1 << 1 == 0;
        let dp = i & 1 << 0 == 0;

        let a = if ap { list[0] } else { -list[0] };
        let b = if bp { list[1] } else { -list[1] };
        let c = if cp { list[2] } else { -list[2] };
        let d = if dp { list[3] } else { -list[3] };

        if a + b + c + d == 7 && ap {
            return format!(
                "{}{}{}{}{}{}{}=7",
                list[0],
                if bp { "+" } else { "-" },
                list[1],
                if cp { "+" } else { "-" },
                list[2],
                if dp { "+" } else { "-" },
                list[3]
            ).to_string();
        }
    }
    panic!("");
}

#[test]
fn test() {
    let tests = vec![("1222", "1+2+2+2=7"), ("0290", "0-2+9+0=7")];
    for (i, (input, output)) in tests.into_iter().enumerate() {
        println!("test:{}", i);
        assert_eq!(run(input.to_string()), output.to_string());
    }
}
