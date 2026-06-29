# Changelog

Changelog for the version-ranges crate.

## v0.2.0 - 2026-06-29

### Breaking

- `Ranges::iter` now returns `(Bound<&V>, Bound<&V>)`, consistent with `Ranges::bounding_range`, instead of references to stored `Bound<V>` values ([#30](https://github.com/astral-sh/pubgrub/pull/30)).

### Added

- Expose `DoubleEndedIterator` from `Ranges::iter` ([#62](https://github.com/astral-sh/pubgrub/pull/62)).
- Add `SetRelation` and `Ranges::relation` for classifying two ranges as equal, subset, disjoint, or overlapping in one pass ([#66](https://github.com/astral-sh/pubgrub/pull/66)).

### Changed

- Short-circuit equality when checking multi-segment subset relationships ([#64](https://github.com/astral-sh/pubgrub/pull/64)).

## v0.1.3 - 2026-04-09

- Add optional `semver` conversions ([#405](https://github.com/pubgrub-rs/pubgrub/pull/405))

## v0.1.2

* Allow `Ranges::contains` to accept borrows, e.g. `&str` for `Ranges<String>`

## v0.1.1

* Added `Ranges::from_iter`
* Implement `IntoIter` on `Ranges`

## v0.1.0

Initial release!
