// SPDX-License-Identifier: MPL-2.0

use pubgrub::{OfflineDependencyProvider, Ranges, State, Term};

type Provider = OfflineDependencyProvider<&'static str, Ranges<u32>>;

#[test]
fn dependency_queries_include_known_ranges_and_both_directions() {
    let mut state = State::<Provider>::init("root", 0);
    let parent = state.package_store.alloc("parent");
    let child = state.package_store.alloc("child");
    let other = state.package_store.alloc("other");

    state.add_dependency(
        parent,
        Ranges::singleton(1u32),
        (child, Ranges::between(2u32, 4u32)),
    );
    state.add_dependency(
        parent,
        Ranges::singleton(2u32),
        (child, Ranges::between(2u32, 4u32)),
    );
    state.add_dependency(child, Ranges::singleton(2u32), (other, Ranges::full()));
    state.add_no_versions(parent, Term::Positive(Ranges::singleton(3u32)));
    state.add_unavailable(
        parent,
        Term::Positive(Ranges::singleton(4u32)),
        "unavailable".into(),
    );

    let dependencies: Vec<_> = state.dependencies(parent).collect();
    assert_eq!(dependencies.len(), 1);
    assert_eq!(dependencies[0].dependent, parent);
    assert_eq!(dependencies[0].dependency, child);
    assert_eq!(
        dependencies[0].dependent_versions,
        &Ranges::singleton(1u32).union(&Ranges::singleton(2u32))
    );
    assert_eq!(
        dependencies[0].dependency_versions,
        Some(&Ranges::between(2u32, 4u32))
    );

    let dependencies: Vec<_> = state.dependencies(child).collect();
    assert_eq!(dependencies.len(), 2);
    assert_eq!(
        (dependencies[0].dependent, dependencies[0].dependency),
        (parent, child)
    );
    assert_eq!(
        (dependencies[1].dependent, dependencies[1].dependency),
        (child, other)
    );
    assert_eq!(state.dependencies(state.root_package).count(), 0);
}

#[test]
fn dependency_queries_handle_empty_and_proxy_constraints() {
    let mut state = State::<Provider>::init("root", 0);
    let proxy = state.package_store.alloc("proxy");
    let base = state.package_store.alloc("base");
    let empty = state.package_store.alloc("empty");

    state.add_proxy_package_incompatibility(proxy, base, Ranges::between(1u32, 3u32));
    state.add_dependency(base, Ranges::singleton(1u32), (empty, Ranges::empty()));

    let dependency = state.dependencies(proxy).next().unwrap();
    assert_eq!(dependency.dependent, proxy);
    assert_eq!(dependency.dependency, base);
    assert_eq!(dependency.dependent_versions, &Ranges::between(1u32, 3u32));
    assert_eq!(
        dependency.dependency_versions,
        Some(&Ranges::between(1u32, 3u32))
    );

    let dependency = state.dependencies(base).last().unwrap();
    assert_eq!(dependency.dependency, empty);
    assert_eq!(dependency.dependency_versions, None);
    // An empty dependency has no negative term and does not require the target package.
    assert_eq!(state.dependencies(empty).count(), 0);
}

#[test]
fn conflict_queries_cover_dependency_rejection_and_propagation() {
    let mut state = State::<Provider>::init("root", 0);
    state.unit_propagation(state.root_package).unwrap();
    state.add_package_version_dependencies(
        state.root_package,
        0,
        Ranges::singleton(0u32),
        [("a", Ranges::full()), ("b", Ranges::full())],
    );
    state.unit_propagation(state.root_package).unwrap();
    let a = state.package_store.alloc("a");
    let b = state.package_store.alloc("b");
    state.add_package_version_dependencies(a, 1, Ranges::singleton(1u32), []);
    assert!(
        state
            .add_package_version_dependencies(
                b,
                1,
                Ranges::singleton(1u32),
                [("a", Ranges::singleton(2u32))],
            )
            .is_none()
    );

    let conflicts: Vec<_> = state.unit_propagation(b).unwrap().into_iter().collect();
    assert!(!conflicts.is_empty());
    for (affected, conflict) in conflicts {
        let packages: Vec<_> = state.conflict_packages(conflict).collect();
        assert!(packages.contains(&affected));
        assert!(!packages.is_empty());
    }

    let conflict = state
        .add_package_version_dependencies(
            b,
            2,
            Ranges::singleton(2u32),
            [("a", Ranges::singleton(2u32))],
        )
        .unwrap();
    assert_eq!(
        state.conflict_packages(conflict).collect::<Vec<_>>(),
        vec![b, a]
    );
    // The learned conflict is not a dependency edge.
    assert_eq!(state.dependencies(b).count(), 2);
}
