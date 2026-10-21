# Change Log
All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

## [Unreleased]
### Added
- `deep-merge-layers` in `common.core` - deep merges an ordered sequence of partial maps (lowest precedence first), dropping nil layers.
- `classify-by-namespace` in `common.core` - returns the value of the first `[ns-prefix value]` entry whose prefix matches a keyword's (dotted) namespace.

### Changed
- Add a new arity to `make-widget-async` to provide a different widget shape.

## [0.1.1] - 2018-11-26
### Changed
- Documentation on how to make the widgets.

### Removed
- `make-widget-sync` - we're all async, all the time.

### Fixed
- Fixed widget maker to keep working when daylight savings switches over.

## 0.1.0 - 2018-11-26
### Added
- Files from the new template.
- Widget maker public API - `make-widget-sync`.

[Unreleased]: https://github.com/your-name/cljc-common/compare/0.1.1...HEAD
[0.1.1]: https://github.com/your-name/cljc-common/compare/0.1.0...0.1.1
