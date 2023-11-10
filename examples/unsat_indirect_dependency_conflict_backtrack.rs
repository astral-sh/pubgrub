// SPDX-License-Identifier: MPL-2.0

use pubgrub::error::PubGrubError;
use pubgrub::range::Range;
use pubgrub::report::{DefaultStringReporter, Reporter};
use pubgrub::solver::{resolve, OfflineDependencyProvider};
use pubgrub::version::SemanticVersion;

fn main() {
    let mut dependency_provider = OfflineDependencyProvider::<&str, SemanticVersion>::new();
    // Define the root package with a dependency on foo between 1.0 and 2.0
    dependency_provider.add_dependencies(
        "root",
        (0, 0, 0),
        vec![("foo", Range::between((1, 0, 0), (2, 0, 0)))],
    );

    // provide foo 1.1 with a satisfiable set
    dependency_provider.add_dependencies("foo", (1, 1, 0), vec![("bar", Range::exact((2, 0, 0)))]);

    // Provide foo 1.2 with unsatisfiable set of direct dependencies
    dependency_provider.add_dependencies(
        "foo",
        (1, 2, 0),
        vec![(
            "bar",
            Range::exact((1, 0, 0)).intersection(&Range::exact((2, 0, 0))),
        )],
    );

    // the resolver should backtrack from 1.2 -> 1.1 but instead fails
    // if 1.1 and 1.2 dependencies are swapped such that 1.2 is satisfiable, no error is raised

    // provide both versions of bar
    dependency_provider.add_dependencies("bar", (1, 0, 0), vec![]);
    dependency_provider.add_dependencies("bar", (2, 0, 0), vec![]);

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
