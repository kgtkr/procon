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

    k回操作後のa,b,cの値をそれぞれ
    fa(k),fb(k),fc(k)
    とする
    a,b,cはfa(0),fb(0),fc(0)
    を表す事とする

    操作を行っていくとa,b,cはそれぞれ
    fa(0),fb(0),fc(0)=a,b,c
    fa(1),fb(1),fc(1)=b+c,a+c,b+c
    fa(2),fb(2),fc(2)=2a+b+c,a+2b+c,a+b+2c
    fa(3),fb(3),fc(3)=2a+3b+3c,3a+2b+3c,3a+3b+2c
    fa(4),fb(4),fc(4)=6a+5b+5c,5a+6b+5c,5a+5b+6c
    となっていく
    
    fa(k),fb(k),fc(k)のa,b,cの係数を
    (fa(k)のaの係数,fa(k)のbの係数,fa(k)のcの係数)(fb(k)のaの係数,fb(k)のbの係数,fb(k)のcの係数)(fc(k)のaの係数,fc(k)のbの係数,fc(k)のcの係数)
    とすると

    k=0: (1,0,0)(0,1,0)(0,0,1)
    k=1: (0,1,1)(1,0,1)(1,1,0)
    k=2: (2,1,1)(1,2,1)(1,1,2)
    k=3: (2,3,3)(3,2,3)(3,3,2)
    k=4: (6,5,5)(5,6,5)(5,5,6)
    となる

    k回操作後の係数を
    (f(k),g(k),g(k))(g(k),f(k),g(k))(g(k),g(k),f(k))
    とすると

    f(0)=1
    g(0)=0
    f(k+1)=g(k)*2
    g(k+1)=f(k)+g(k)
    という連立漸化式を得られる

    これを解くと
    f(k)=1/3(2(-1)^k+2^k)
    g(k)=1/3((-1)^(1+k)+2^k)

    また、fa(k),fb(k),fc(k)の値は
    fa(k)=f(k)a+g(k)b+g(k)c
    fb(k)=g(k)a+f(k)b+g(k)c
    fc(k)=g(k)a+g(k)b+f(k)c
    である
    
    求める値は
    fa(k)-fb(k)
    =(f(k)a+g(k)b+g(k)c)-(g(k)a+f(k)b+g(k)c)
    =f(k)a+g(k)b-g(k)a-f(k)b
    =f(k)(a-b)+g(k)(b-a)
    =1/3(2(-1)^k+2^k)(a-b)+1/3((-1)^(1+k)+2^k)(b-a))
    =(-1)^k(a-b)

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
