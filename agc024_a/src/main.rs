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
    if k % 2 == 0 { -b + a } else { b - a }.to_string()

    /*
    考察
    http://www.wolframalpha.com/
    で漸化式解ける、凄い

    操作を行っていくと
    a,b,c
    b+c,a+c,b+c
    2a+b+c,a+2b+c,a+b+2c
    となっていく
    
    係数を考える
    (1,0,0)(0,1,0)(0,0,1)
    (0,1,1)(1,0,1)(1,1,0)
    (2,1,1)(1,2,1)(1,1,2)
    (2,3,3)(3,2,3)(3,3,2)
    (6,5,5)(5,6,5)(5,5,6)

    aについてのみ考える(b,cも同じように考えれるため)
    (x[k],y[k],y[k])
    とすると

    x[1]=1
    y[1]=0
    x[k+1]=y[k]*2
    y[k+1]=x[k]+y[k]
    という連立漸化式を得られる
    
    この連立漸化式を隣接三項間漸化式に直す
    x[1]=1
    x[2]=0
    x[k+2]=x[k+1]+x[k]*2

    y[1]=0
    y[2]=1
    y[k+2]=y[k+1]+y[k]*2

    これを解くと
    x[k]=1/6(2^k-4(-1)^k)
    y[k]=1/6(2(-1)^k+2^k)

    b,cについても同様に成り立つ

    よって、k回操作を行った時のa,b,cの値はそれぞれ
    k回操作後のa: x[k]a+y[k]b+y[k]c
    k回操作後のb: y[k]a+x[k]b+y[k]c
    k回操作後のc: y[k]a+y[k]b+x[k]c
    となる
    
    求める値はk回操作後のa-bなので
    (x[k]a+y[k]b+y[k]c)-(y[k]a+x[k]b+y[k]c)
    =x[k]a+y[k]b-y[k]a-x[k]b
    =x[k](a-b)+y[k](b-a)
    =1/6((2^k-4(-1)^k)(a-b)+(2(-1)^k+2^k)(b-a))
    =b(-1)^k-a(-1)^k
    =(-1)^k(b-a)

    よって
    kが偶数の時:a-b
    kが奇数の時:b-a
    */
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
    test1: "1 2 3 1" => "1",
    test2: "2 3 2 0" => "-1",
    test3: "1000000000 1000000000 1000000000 1000000000000000000" => "0",
}
