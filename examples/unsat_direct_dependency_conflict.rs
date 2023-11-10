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

    // Provide the needed versions of foo
    dependency_provider.add_dependencies("foo", (1, 0, 0), vec![]);
    dependency_provider.add_dependencies("foo", (2, 0, 0), vec![]);

    // Run the algorithm
    match resolve(&dependency_provider, "root", (0, 0, 0)) {
        Ok(sol) => println!("{:?}", sol),
        Err(PubGrubError::NoSolution(mut derivation_tree)) => {
            eprintln!("No solution.\n");

            eprintln!("### Default report:");
            eprintln!("```");
            eprintln!("{}", DefaultStringReporter::report(&derivation_tree));
            eprintln!("```\n");

            derivation_tree.collapse_no_versions();
            eprintln!("### Report with `collapse_no_versions`:");
            eprintln!("```");
            eprintln!("{}", DefaultStringReporter::report(&derivation_tree));
            eprintln!("```");
            std::process::exit(1);
        }
        Err(err) => panic!("{:?}", err),
    };
}
