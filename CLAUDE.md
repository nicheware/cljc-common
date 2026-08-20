# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project

`nicheware/cljc-common` is a Leiningen library of utility functions written entirely in `.cljc` so every
namespace compiles for both Clojure (JVM) and ClojureScript. There is no application here — only the
library, its tests, and generated API docs.

## Commands

```bash
lein test                                   # all Clojure (JVM) tests
lein test nicheware.platform.utilities.common.core-test          # one namespace
lein test :only nicheware.platform.utilities.common.core-test/test-slice   # one deftest

lein doo chrome test once                   # ClojureScript tests (headless Chrome via lein-doo)
lein doo chrome test                        # ... in auto-recompile watch mode

lein codox                                  # regenerate ./codox API docs (uses the +codox profile alias)
lein install                                # install jar to local ~/.m2
```

`lein test` runs under the `:test` profile, which puts `env/test/resources` and `test/resources` on the
classpath — the config tests depend on this (see below).

## Cross-platform rules

- All library source lives in `src/cljc/`; there is no `src/clj` or `src/cljs`. Platform differences are
  handled with reader conditionals inside the `.cljc` file, not with separate files.
- Tests mirror this: `test/cljc/` holds the real tests as `.cljc`, run on both platforms.
- The ClojureScript test build (`:cljsbuild` id `"test"`) compiles `src/cljc`, `test/cljc` and `test/cljs`,
  with `test/cljs/.../client/test_runner.cljs` as its `:main`. **When you add a new test namespace you must
  also add it to `test_runner.cljs`** — in both the `:require` vector and the `doo-tests` call — or it will
  silently never run under ClojureScript.
- Clojure-only code (anything needing `clojure.java.io`, JVM classes, or `eval`) is wrapped in a top-level
  `#?(:clj (defn ...))`. `config/read-config` and `config/load-config` are the main examples; their
  ClojureScript counterparts are the HTTP variants `read-http-config` / `load-http-config`, which return
  promesa promises rather than values.

## Namespace map

All namespaces live under `nicheware.platform.utilities.common`:

- `core` — the base namespace; complements `clojure.core` (slicing, collection insert/replace, map
  transforms, searching, string helpers, and the cross-platform primitives `rand-uuid`, `parse-int`,
  `current-time-millis`, `edn-read`). Nearly every other namespace depends on it (notably `deep-merge`).
- `math` — float/number helpers.
- `graphics` plus `graphics.color`, `graphics.line`, `graphics.interpolate` — drawing/geometry maths.
- `version` — in-memory versioning of an "asset map" of the shape
  `{<asset-key> {:current <modified-time> :name <asset-key> :versions {<modified-time> <asset> ...}}}`.
  Each version is a full copy of state; keys are timestamps.
- `state.migration` — migrates a state map carrying a `:version` string forward, driven by a migration map
  keyed by major-minor version with `{:from-version :to-version :migration-fns [...]}`.
- `config` — merges a common `config.edn` with an environment-specific `env-config.edn` (deep-merge, env
  wins), then recursively resolves `{{mustache}}` variables and section functions against the merged map
  itself until no variables remain. Defaults are overridable via an `opts` map
  (`:env-file-name :common-file-name :env-path :common-path :resolve-vars`).

## Documentation conventions

The namespace docstring is the primary documentation surface — codox publishes it as the namespace page.
Follow the existing pattern: a markdown table grouping functions with `[[wikilink]]` references, then
worked examples in fenced ```clojure blocks. Function docstrings list args and the return as a `-` bullet
list. Internal helpers are marked `^:no-doc` rather than made private, so tests can reach them.

`README.md`, `doc/intro.md`, and every namespace docstring carry overlapping namespace tables; when adding
or renaming a namespace, update all of them.

## Test notes

- `config_test` starts a real Jetty server on port 9991 in a `:once` fixture (Clojure only) serving
  `test/resources/public/...` and `env/test/resources/public/...`, so the HTTP config functions can be
  tested end to end. That port must be free.
- The dependency set is pinned to old versions (Clojure 1.9.0-alpha17, ClojureScript 1.9.562). The `:dev`
  profile carries `javax.xml.bind/jaxb-api` purely so the old ClojureScript compiler loads on JDK 11+;
  it is deliberately kept out of the published pom. Do not bump deps casually.

## Style

Coding style follows [nicheware/clj-guidelines](http://github.com/nicheware/clj-guidelines). Observable
conventions: section banner comments (`;; ====== Section ======`) dividing each namespace, multi-arity
functions where an options map has defaults, and `cond->` chains for optional pipeline steps.
