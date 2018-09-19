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
        .split("\n")
        .skip(1)
        .map(|x| {
            x.chars()
                .map(|x| if x == '.' { true } else { false })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    let path = bfs(&list);
    //白の数
    let count = list.into_iter().flat_map(|x| x).filter(|x| *x).count();
    if let Some(len) = path.map(|x| x.len()) {
        (count - len).to_string()
    } else {
        "-1".to_string()
    }
}

#[test]
fn test() {
    let tests = vec![
        (
            "3 3
..#
#..
...",
            "2",
        ),
        (
            "10 37
.....................................
...#...####...####..###...###...###..
..#.#..#...#.##....#...#.#...#.#...#.
..#.#..#...#.#.....#...#.#...#.#...#.
.#...#.#..##.#.....#...#.#.###.#.###.
.#####.####..#.....#...#..##....##...
.#...#.#...#.#.....#...#.#...#.#...#.
.#...#.#...#.##....#...#.#...#.#...#.
.#...#.####...####..###...###...###..
.....................................",
            "209",
        ),
    ];
    for (i, (input, output)) in tests.into_iter().enumerate() {
        println!("test:{}", i);
        assert_eq!(run(input.to_string()), output.to_string());
    }
}

#[derive(PartialEq, Debug, Clone)]
enum Cell {
    Wall,
    Empty,
    Visited(usize, usize),
}
fn bfs(m: &Vec<Vec<bool>>) -> Option<Vec<(usize, usize)>> {
    let mut m = m.iter()
        .map(|x| {
            x.iter()
                .map(|&x| if x { Cell::Empty } else { Cell::Wall })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    let goal = (m.len() - 1, m[0].len() - 1);
    let mut n: (usize, usize) = (0, 0);
    // Queue に初期値を積む
    let mut queue = vec![n];
    while queue.len() > 0 {
        // queueから取り出す
        n = queue[0];
        queue.remove(0);
        // 行けるセルとをチェック
        for d in vec![(1i32, 0i32), (0i32, 1i32), (-1i32, 0i32), (0i32, -1i32)] {
            let next_x = (n.0 as i32) + d.0;
            let next_y = (n.1 as i32) + d.1;
            if next_x >= 0 && next_x <= (goal.0 as i32) && next_y >= 0 && next_y <= (goal.1 as i32)
            {
                let next_x = next_x as usize;
                let next_y = next_y as usize;

                if m[next_x][next_y] == Cell::Empty {
                    m[next_x][next_y] = Cell::Visited(n.0, n.1);
                    // Queueに積む
                    queue.push((next_x, next_y))
                }

                if (next_x, next_y) == goal {
                    m[0][0] = Cell::Empty;
                    let mut path = vec![(goal.0, goal.1)];
                    let mut c = &m[goal.0][goal.1];
                    while let &Cell::Visited(x, y) = c {
                        path.push((x, y));
                        c = &m[x][y];
                    }
                    path.reverse();
                    return Some(path);
                }
            }
        }
    }
    None
}
