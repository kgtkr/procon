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


//オーディオの分散
fn bun(name: &str) -> i64 {
    let mut reader = hound::WavReader::open(name).unwrap();
    let vec = reader
        .samples::<i16>()
        .map(|x| x.unwrap().abs())
        .collect::<Vec<_>>();

    let sum = vec.iter().fold(0i64, |sum, &i| sum + i as i64);
    let count = vec.len() as i64;
    let ave = sum / count;
    vec.iter().map(|&x| (x as i64 - ave).pow(2)).sum::<i64>() / count
}
