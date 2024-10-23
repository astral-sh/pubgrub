//! Sudoku solver with a slower encoding. Good for benchmarking a slow cases.
// SPDX-License-Identifier: MPL-2.0

use std::fmt;

use pubgrub::{
    resolve, DefaultStringReporter, OfflineDependencyProvider, PubGrubError, Range, Reporter,
    SelectedDependencies,
};

type DP = OfflineDependencyProvider<Constraint, Range<u8>>;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
enum Constraint {
    /// The version is the value of the cell.
    Cell { row: u8, col: u8 },
    /// The version is the is col of the cell which has the desired row value.
    Row { row: u8, has: u8 },
    /// The version is the is row of the cell which has the desired col value.
    Col { col: u8, has: u8 },
    /// The version is the is mod-wrapped position of the cell which has the desired row block value.
    Block { nub: u8, has: u8 },
    /// This constraint enforces picking all values for all rows, cols and blocks.
    Rools,
    /// The root constraint sets the known values.
    Board,
}

impl fmt::Display for Constraint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        format!("{self:?}").fmt(f)
    }
}

fn from_board(b: &str) -> Vec<(Constraint, Range<u8>)> {
    let mut out = vec![(Constraint::Rools, Range::full())];
    for (row, line) in b
        .trim()
        .lines()
        .map(str::trim)
        .filter(|l| !l.starts_with('-'))
        .enumerate()
    {
        let row: u8 = (row + 1).try_into().unwrap();
        for (col, val) in line
            .split_ascii_whitespace()
            .filter(|c| !c.starts_with('|'))
            .enumerate()
        {
            let col: u8 = (col + 1).try_into().unwrap();
            if let Some(val) = val.chars().next().unwrap().to_digit(10) {
                out.push((Constraint::Cell { row, col }, Range::singleton(val as u8)));
            }
        }
    }
    out
}

fn to_board(res: SelectedDependencies<DP>) -> String {
    let mut out = String::new();
    for row in 1..=9 {
        for col in 1..=9 {
            out.push_str(&res[&Constraint::Cell { row, col }].to_string());
            out.push(' ');
            if col == 3 || col == 6 {
                out.push_str("| ");
            }
        }
        out.push('\n');
        if row == 3 || row == 6 {
            out.push_str("------+-------+-------\n");
        }
    }
    out
}

fn main() {
    env_logger::init();

    let mut dependency_provider = DP::new();
    let mut to_solve = vec![];
    for row in 1..=9 {
        for col in 1..=9 {
            for val in 1..=9 {
                let name = Constraint::Cell { row, col };
                to_solve.push((name.clone(), Range::full()));
                dependency_provider.add_dependencies(name, val, [])
            }
        }
    }
    for row in 1..=9 {
        for val in 1..=9 {
            let name = Constraint::Row { row, has: val };
            to_solve.push((name.clone(), Range::full()));
            for col in 1..=9 {
                dependency_provider.add_dependencies(
                    name.clone(),
                    col,
                    [(Constraint::Cell { row, col }, Range::singleton(val))],
                )
            }
        }
    }
    for col in 1..=9 {
        for val in 1..=9 {
            let name = Constraint::Col { col, has: val };
            to_solve.push((name.clone(), Range::full()));
            for row in 1..=9 {
                dependency_provider.add_dependencies(
                    name.clone(),
                    row,
                    [(Constraint::Cell { row, col }, Range::singleton(val))],
                )
            }
        }
    }
    for block in 0..=8 {
        for val in 1..=9 {
            let name = Constraint::Block {
                nub: block,
                has: val,
            };
            to_solve.push((name.clone(), Range::full()));
            for cell in 0..=8 {
                let row = (3 * (block / 3)) + cell / 3 + 1;
                let col = (3 * (block % 3)) + cell % 3 + 1;
                dependency_provider.add_dependencies(
                    name.clone(),
                    cell,
                    [(Constraint::Cell { row, col }, Range::singleton(val))],
                )
            }
        }
    }
    dependency_provider.add_dependencies(Constraint::Rools, 0, to_solve);

    dependency_provider.add_dependencies(
        Constraint::Board,
        0,
        from_board(
            r#"
        5 3 _ | _ 7 _ | _ _ _
        6 _ _ | 1 9 5 | _ _ _
        _ 9 8 | _ _ _ | _ 6 _
       -------+-------+-------
        8 5 9 | _ 6 1 | 4 2 3
        4 2 6 | 8 5 3 | 7 9 1
        7 1 3 | 9 2 4 | 8 5 6
       -------+-------+-------
        _ 6 _ | _ _ _ | 2 8 _
        _ _ _ | 4 1 9 | _ _ 5
        _ _ _ | _ 8 6 | 1 7 9"#,
        ),
    );

    let start = std::time::Instant::now();
    match resolve(&dependency_provider, Constraint::Board, 0) {
        Ok(sol) => println!("{}", to_board(sol)),
        Err(PubGrubError::NoSolution(mut derivation_tree)) => {
            derivation_tree.collapse_no_versions();
            eprintln!("{}", DefaultStringReporter::report(&derivation_tree));
            std::process::exit(1);
        }
        Err(err) => panic!("{:?}", err),
    };
    println!("Time taken: {:?}", start.elapsed().as_secs_f32());

    dependency_provider.add_dependencies(
        Constraint::Board,
        1,
        from_board(
            r#"
        5 3 _ | _ 7 _ | _ _ _
        6 _ _ | 1 9 5 | _ _ _
        _ 9 8 | _ _ _ | _ 6 _
       -------+-------+-------
        8 _ _ | _ 6 _ | _ _ 3
        4 _ _ | 8 _ 3 | _ _ 1
        7 _ _ | _ 2 _ | _ _ 6
       -------+-------+-------
        _ 6 _ | _ _ _ | 2 8 _
        _ _ _ | 4 1 9 | _ _ 5
        _ _ _ | _ 8 _ | _ 7 9"#,
        ),
    );

    let start = std::time::Instant::now();
    match resolve(&dependency_provider, Constraint::Board, 1) {
        Ok(sol) => println!("{}", to_board(sol)),
        Err(PubGrubError::NoSolution(mut derivation_tree)) => {
            derivation_tree.collapse_no_versions();
            eprintln!("{}", DefaultStringReporter::report(&derivation_tree));
            std::process::exit(1);
        }
        Err(err) => panic!("{:?}", err),
    };
    println!("Time taken: {:?}", start.elapsed().as_secs_f32());
}
