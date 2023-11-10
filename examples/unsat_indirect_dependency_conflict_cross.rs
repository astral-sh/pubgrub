// SPDX-License-Identifier: MPL-2.0

use pubgrub::error::PubGrubError;
use pubgrub::range::Range;
use pubgrub::report::{DefaultStringReporter, Reporter};
use pubgrub::solver::{resolve, OfflineDependencyProvider};
use pubgrub::version::SemanticVersion;

fn main() {
    let mut dependency_provider = OfflineDependencyProvider::<&str, SemanticVersion>::new();
    // Define the root package with a dependency on foo and bar
    dependency_provider.add_dependencies(
        "root",
        (0, 0, 0),
        vec![
            ("foo", Range::exact((1, 0, 0))),
            ("bar", Range::exact((1, 0, 0))),
        ],
    );

    // foo depends on foobar 1.0
    dependency_provider.add_dependencies(
        "foo",
        (1, 0, 0),
        vec![("foobar", Range::exact((1, 0, 0)))],
    );

    // bar depends on the conflicting foobar 2.0
    dependency_provider.add_dependencies(
        "bar",
        (1, 0, 0),
        vec![("foobar", Range::exact((2, 0, 0)))],
    );

    // provide both versions of foobar
    dependency_provider.add_dependencies("foobar", (1, 0, 0), vec![]);
    dependency_provider.add_dependencies("foobar", (2, 0, 0), vec![]);

    // Run the algorithm
    match resolve(&dependency_provider, "root", (0, 0, 0)) {
        Ok(sol) => println!("{:?}", sol),
        Err(PubGrubError::NoSolution(mut derivation_tree)) => {
            eprintln!("No solution.\n");
            eprintln!("{}", DefaultStringReporter::report(&derivation_tree));
            std::process::exit(1);
        }
        Err(err) => panic!("{:?}", err),
    };
}
