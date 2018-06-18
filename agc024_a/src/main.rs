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
    input!(input=>(a:i64 b:i64 c:i64 k:i64));
    let n = a + b;
    /*
    (1,0,0)(0,1,0)(0,0,1)
    (0,1,1)(1,0,1)(1,1,0)
    (2,1,1)(1,2,1)(1,1,2)
    (2,3,3)(3,2,3)(3,3,2)
    (6,5,5)(5,6,5)(5,5,6)
    */
    /*
    aについて
    (x[i],y[i],y[i])
    とすると
    x[1]=1
    y[1]=0

    (x[i],y[i])
    (1,0)
    (0,1)
    (2,1)
    (2,3)
    (6,5)

    x[i+1]=y[i]*2
    y[i+1]=x[i]+y[i]
    
    x[1]=1
    y[1]=0
    x[i+2]=x[i+1]+x[i]*2
    y[i+2]=y[i+1]+y[i]*2


    b,cについても同様に成り立つ
    */
    n.to_string()
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
    test1: "3 9" => "12",
    test2: "31 32" => "63",
    test3: "1 2" => "3",
    test4: "-1 2" => "1",
    test5: "10 1" => "11",
}
