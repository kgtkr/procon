extern crate hound;

fn main() {
    println!("あり {}", bun("sample_mono_comp.wav"));
    println!("なし {}", bun("sample_mono_dry.wav"));
    for x in 1..16 {
        let a = bun(&format!("input/{:>02}_A.wav", x));
        let b = bun(&format!("input/{:>02}_B.wav", x));
        print!("{}", if a > b { "B" } else { "A" });
    }
}


//オーディオの変化
fn bun(name: &str) -> f64 {
    let mut reader = hound::WavReader::open(name).unwrap();
    let vec = reader
        .samples::<i32>()
        .map(|x| x.unwrap())
        .collect::<Vec<_>>();

    vec.iter()
        .enumerate()
        .map(|(i, &x)| match i {
            0 => 0i64,
            _ => ((x as i64) - (vec[i - 1] as i64)).abs(),
        })
        .fold(0i64, |sum, i| sum + i as i64) as f64 / ave(&vec)
}

fn ave(vec: &Vec<i32>) -> f64 {
    let sum = &vec.iter()
        .map(|x| x.abs())
        .fold(0f64, |sum, i| sum + i as f64);
    sum / vec.len() as f64
}
