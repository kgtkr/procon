extern crate hound;

fn main() {
    println!("あり {}", onryo("sample_mono_comp.wav"));
    println!("なし {}", onryo("sample_mono_dry.wav"));

    for x in 1..16 {
        let a = onryo(&format!("input/{:>02}_A.wav", x));
        let b = onryo(&format!("input/{:>02}_B.wav", x));
        print!("{}", if a > b { "A" } else { "B" });
    }
}


//音量の絶対値の合計
fn onryo(name: &str) -> i64 {
    let mut reader = hound::WavReader::open(name).unwrap();
    let vec = reader
        .samples::<i16>()
        .map(|x| (x.unwrap() as i32).abs())
        .collect::<Vec<_>>();

    vec.iter().fold(0i64, |sum, &i| sum + i as i64)
}
