extern crate core;

use std::io::{self, Read};

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
    let output = run(input.trim().to_string());
    println!("{}", output);
}

fn run(input: String) -> String {
    input!(input=>
        (n:usize)
        (c:[i64])
        (q:usize)
        {q;list:(i64,usize,i64)});
    let mut apb = Vec::new();
    for _ in 0..n {
        apb.push(0);
    }

    for (t, x, d) in list {
        if t == 1 {
            //収穫
            apb[x - 1] += d;
            if apb[x - 1] > c[x - 1] {
                return x.to_string();
            }
        } else {
            //出荷
            apb[x - 1] -= d;
            if apb[x - 1] < 0 {
                return x.to_string();
            }
        }
    }

    0.to_string()
}

macro_rules! tests {
    ($($name:ident: $input:expr=>$output:expr,)*) => {
        mod tests {
            $(
                #[test]
                fn $name() {
                    assert_eq!(super::run($input.to_string()), $output.to_string());
                }
            )*
        }
    }
}

tests! {
    test1: "2
3 3
4
1 1 2
1 2 3
2 1 3
2 2 3" => "1",
    test2: "2
3 3
4
1 1 3
2 1 2
1 2 3
1 1 3" => "1",
    test3: "3
3 4 5
4
1 1 3
1 2 3
1 3 5
2 2 2" => "0",
    test4: "6
28 56 99 3 125 37
10
1 1 10
1 1 14
1 3 90
1 5 10
2 3 38
2 1 5
1 3 92
1 6 18
2 5 9
2 1 4" => "3",
}
