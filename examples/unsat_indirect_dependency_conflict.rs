// SPDX-License-Identifier: MPL-2.0

use pubgrub::error::PubGrubError;
use pubgrub::range::Range;
use pubgrub::report::{DefaultStringReporter, Reporter};
use pubgrub::solver::{resolve, OfflineDependencyProvider};
use pubgrub::version::SemanticVersion;

type SemVS = Range<SemanticVersion>;

fn main() {
    let mut dependency_provider = OfflineDependencyProvider::<&str, SemVS>::new();
    // Define the root package with a dependency
    dependency_provider.add_dependencies(
        "root",
        (0, 0, 0),
        vec![("foo", Range::singleton((1, 0, 0)))],
    );

    dependency_provider.add_dependencies(
        "foo",
        (1, 0, 0),
        vec![
            ("bar", Range::singleton((1, 0, 0))),
            ("bar", Range::singleton((2, 0, 0))),
        ],
    );

    // provide both versions of bar
    dependency_provider.add_dependencies("bar", (1, 0, 0), vec![]);
    dependency_provider.add_dependencies("bar", (2, 0, 0), vec![]);

    // in practice, foo must be collapsed by the user
    dependency_provider.add_dependencies(
        "foo",
        (1, 0, 0),
        vec![(
            "bar",
            Range::singleton((1, 0, 0)).intersection(&Range::singleton((2, 0, 0))),
        )],
    );

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
