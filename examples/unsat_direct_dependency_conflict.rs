// SPDX-License-Identifier: MPL-2.0

use pubgrub::error::PubGrubError;
use pubgrub::range::Range;
use pubgrub::report::{DefaultStringReporter, Reporter};
use pubgrub::solver::{resolve, OfflineDependencyProvider};
use pubgrub::version::SemanticVersion;

fn main() {
    let mut dependency_provider = OfflineDependencyProvider::<&str, SemanticVersion>::new();
    // Define the root package with incompatible versions for a single dependency
    dependency_provider.add_dependencies(
        "root",
        (0, 0, 0),
        vec![
            ("foo", Range::exact((1, 0, 0))),
            ("foo", Range::exact((2, 0, 0))),
        ],
    );

    // Run the algorithm
    match resolve(&dependency_provider, "root", (0, 0, 0)) {
        Ok(sol) => println!("{:?}", sol),
        Err(PubGrubError::NoSolution(mut derivation_tree)) => {
            eprintln!("No solution.");
            eprintln!("\nReport with no versions:");
            eprintln!("{}", DefaultStringReporter::report(&derivation_tree));

            eprintln!("\nReport with collapse no versions:");
            derivation_tree.collapse_no_versions();
            eprintln!("{}", DefaultStringReporter::report(&derivation_tree));
            std::process::exit(1);
        }
        Err(err) => panic!("{:?}", err),
    };
}
