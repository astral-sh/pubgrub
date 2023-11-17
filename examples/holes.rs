use pubgrub::error::PubGrubError;
use pubgrub::range::Range;
use pubgrub::report::{DefaultStringReporter, Reporter};
use pubgrub::solver::{resolve, OfflineDependencyProvider};
use pubgrub::version::SemanticVersion;

fn main() {
    let mut dependency_provider = OfflineDependencyProvider::<&str, Range<SemanticVersion>>::new();

    // root depends on foo...
    dependency_provider.add_dependencies("root", (1, 0, 0), vec![("foo", Range::full())]);

    for i in 1..20 {
        // foo depends on bar...
        dependency_provider.add_dependencies("foo", (i, 0, 0), vec![("bar", Range::full())]);
    }

    match resolve(&dependency_provider, "root", (1, 0, 0)) {
        Ok(sol) => println!("{:?}", sol),
        Err(PubGrubError::NoSolution(mut derivation_tree)) => {
            derivation_tree.collapse_no_versions();
            eprintln!("{}", DefaultStringReporter::report(&derivation_tree));
        }
        Err(err) => panic!("{:?}", err),
    };
}
