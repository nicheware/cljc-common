# Claude Prompts

## Table of Contents

- [1. Core](#1-core)
  - [Prompt - debug macro](#prompt---debug-macro)
  - [Summary](#summary)
  - [Prompt - use debug instead or println](#prompt---use-debug-instead-or-println)
  - [Summary](#summary-1)
## 1. Core
### Prompt - debug macro
> Create a new macro called "debug" in core.clj which satisfies the following goal:
>
> A macro for outputting debug that has a first argument as a function name string, and a list of arguments (which will be expressions or vairables in scope) to be output.
>
> It will format all output as follows:
>
>   debug: (<name-of-thread>) [<current-date-time>]: <last-two-namespaces-of-current-namespace>/<function-name>(): <arg1-name>: <arg1-value>, ....
>
>
> An example is:
>
> (defn slice
>  [coll range]
>  (debug "slice" coll range)
>
> would produce
>
>   debug: (nREPL-agent-3456) [Tue Sep 12 12:49:08 AEST 2023]: commons.core/slice-wrap(): coll 22 range 34
>
> The  macro should only produce output an environment varaible COMMON_DEBUG is defined to be true and one or more of the conditions below.
>
>   export COMMON_DEBUG='true'
>
> Once it is set additional checks are required before debug is output. Either
> - meta data in the current namespace includes {:debug true} key and value. {:debug false} means debug is off, Missing means it is off.
> - the debug config file name has been defined in a variable, and the namespace is turned on in that config file. This will
>   override any meta data debug.
>
> Config file operation:
>
>   export COMMON_DEBUG_CONFIG='~/debug-config.edn'
>
> The file can  contains a list namespace wildcards (using regexp): eg:
>
>   {:debug-on ['com.transacumen.stratum*'
>               'com.trasacumen.commons']}
>
> This turns debug on for the matched namespaces.
>
> The aim is to have all debug statements compiled out, if the debug is turned off for this namespace, so has no runtime overhead.
> Normally debug would only be turned on in a development environment, but normally not part of a CI build and never in a production
> runtime.

### Summary
I've added the debug macro to src/cljc/nicheware/platform/utilities/common/core.cljc, in a new "Debug macro" section after cond-t.
The Clojure tests pass (84 tests, 0 failures), and a real run with the environment variables set works. I haven't run the ClojureScript tests,
because Karma isn't installed on this machine. The ClojureScript build does compile.

Output:
debug: (main) [Fri Sep 25 14:55:07 AEST 2026]: common.core/slice(): coll: [1 2 3], range: {:start 1}, (count coll): 3
This follows your format spec (name: value, ...) rather than the looser coll 22 range 34 in your example. Values are printed as Clojure data,
so strings keep their quotes and nil shows as nil. Each argument is labelled with its source code, so an expression like (count coll) works as well as a variable.

When output is produced:
- COMMON_DEBUG must be true. Otherwise every debug call is compiled out.
- Then either the namespace has {:debug true} metadata ((ns ^{:debug true} my.ns ...) or (ns my.ns {:debug true} ...)), or
  the namespace matches an entry in the COMMON_DEBUG_CONFIG file. A config match turns debug on even if the metadata says {:debug false}. A namespace the config doesn't match falls back to its metadata.
- The ~ in the config path is expanded, since your single-quoted export stops the shell from doing it.
- Your example config works as written: the single-quoted entries are read as symbols and the quotes are removed.
- Compiled out: when debug is off, the call becomes nil, so the arguments are never evaluated and there's no runtime cost. The end-to-end run confirmed this.

Decisions to check:
- How patterns match: each entry is a regex matched against the start of the namespace name. So com.nicheware.common* matches com.nicheware.common.core,
  and com.nicheware.common also matches com.nicheware.commons.util. Add a trailing $ for an exact match. A full-string match would have made your pattern*
  example miss the namespaces under it.
- Changes need a recompile: the on/off decision is made when the code is compiled. Changing the environment variables, the config file or the metadata only takes effect when the
  namespace is recompiled or reloaded.
- Bad config file: a missing or invalid file prints one warning and is treated as no config, rather than breaking the build.
- ClojureScript: core now loads its own macros under ClojureScript, so (:require [...common.core :refer [debug]]) works. The thread is always shown as main,
   and the date uses JavaScript's format. I haven't checked that {:debug true} metadata is picked up in ClojureScript; the config-file route doesn't depend on it.

I added tests to core_test.cljc and a note about the macro to CLAUDE.md. Nothing is committed yet.

- install karam

Karma is installed, and the ClojureScript tests now run: lein doo chrome test once gives 129 of 129 passed, 0 failures. That covers all 9 test namespaces in the test runner, including core-test with the new debug tests.

I installed it globally with npm under your nvm Node (v18.19.1), so it didn't need sudo and doesn't change the repo:

npm install -g karma karma-cli karma-cljs-test karma-chrome-launcher   # karma 6.4.4

- Tied to this Node version: the install belongs to Node 18.19.1. If you switch nvm versions, karma won't be on your PATH until you rerun the same install.
- Test runner gap: config_test isn't in test_runner.cljs, so it never runs under ClojureScript. That was already the case before this change.

### Prompt - use debug instead or println

> Can you modify all println which are actually just debug to use the new "debug" macro, in this repo
> If they are commented out, then dont comment out the debug macro.

### Summary

Both clean builds pass with 184 tests and no failures, so the 129 came from the stale incremental build. With debug off, the ClojureScript build prints no debug output. The build
warnings (load-config, cljstache) aren't from this change: the clean build of the unchanged branch shows the same load-config warnings.

Warning about your editor: to get that baseline I briefly stashed and restored the working tree. Afterwards I noticed an Emacs lock file (.#core.cljc), which means core.cljc is
open in Emacs. The files on disk are correct, but revert that buffer before saving, or you could write back an older version over these changes.

I converted every debug println into a live, uncommented debug call:

- src: 29 calls across core, version, state.migration, graphics.interpolate, graphics.color and graphics.line. I corrected a few wrong labels while converting:
  - find-list is now find-last.
  - One of the two find-index-by-pred labels was really find-indexes-by-pred.
  - find-index now prints coll too; the old label named it but never printed it.
- add-version: the old dummy (println ...) let binding is now a normal debug call in the function body.
- Tests: the Jetty fixture's "Before test"/"After test" messages in config_test, the three "Lerp" prints in line_test, and the print in hello_test.cljs.

Other changes:
- Macro change: a string literal argument is now printed once, unlabelled, as a message. For example, (debug "http-server-fixture" "Before test")
  prints ...config-test/http-server-fixture(): Before test. Before, it would have printed "Before test": "Before test". This change has tests.
- Moved section: the debug section now sits at the top of core.cljc so core's own functions can use it. Under ClojureScript, core also refers debug to itself.
- Left alone: the printlns in the (comment ...) scratch block at the end of core_test.cljc. That's REPL experiment code, where printing is the point.

Test results with debug off:
- Clojure: 189 tests, 0 failures.
- ClojureScript: 184 tests passed.

Test results with debug on: I set COMMON_DEBUG=true and pointed the config at {:debug-on ["nicheware"]}. Both suites still pass, and every converted call site printed a debug line on both platforms. That confirms each one compiles when enabled and prints what it should.

- ignore prompts.md

I created .claude/settings.json, which denies both reading and editing PROMPTS.md:

{
  "permissions": {
    "deny": ["Read(**/PROMPTS.md)", "Edit(**/PROMPTS.md)"]
  }
}

I checked it: an attempt to read PROMPTS.md was refused by the permission settings. The rule also stops search tools from looking inside the file.

Two notes:
- Shared file: unlike settings.local.json, this file is meant to be committed, so the rule applies to anyone using Claude Code on the repo. It isn't committed yet.
- Shell commands aren't covered: the rule doesn't stop commands like cat PROMPTS.md. The CLAUDE.md note covers that case.
