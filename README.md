# cljc-common

A Clojure library utility functions that can be called from either a Clojure or Clojurescript application.

## Installation

Add the following dependency to your `project.clj` file:

    [nicheware/cljc-common "1.0.0"]

## Documentation

* [API Docs](http://nicheware.github.io/cljc-common)

## Usage

The functions within the cljc-common library  are categorized by namespace as follows:

|Namespace|Description|
|---|---|
|[core](http://nicheware.github.io/cljc-common/nicheware.platform.utilities.common.core.html)|Functions that complement those in clojure.core, operating on the main clojure collections.|
|[math](http://nicheware.github.io/cljc-common/nicheware.platform.utilities.common.math.html)|Simple mathematical functions and functions dealing with floats.|
|[graphics](http://nicheware.github.io/cljc-common/nicheware.platform.utilities.common.graphics.html) |Functions useful in graphics and drawing calculations.|
|[graphics.color](http://nicheware.github.io/cljc-common/nicheware.platform.utilities.common.graphics.color.html) | Functions for color calculations and color model conversions.|
|[graphics.line](http://nicheware.github.io/cljc-common/nicheware.platform.utilities.common.graphics.line.html)|Simple line equation and point on line functions.|
|[graphics.interpolate](http://nicheware.github.io/cljc-common/nicheware.platform.utilities.common.graphics.interpolate.html)|Functions for performing interpolation between points and control points.|
|[state.migration](http://nicheware.github.io/cljc-common/nicheware.platform.utilities.common.state.migration.html)|Functions for migrating Clojure maps, used to represent application state, between state versions. |
|[version](http://nicheware.github.io/cljc-common/nicheware.platform.utilities.common.version.html)|Functions dealing with references to assets/data and historic versions of assets/data.|
|[config](http://nicheware.github.io/cljc-common/nicheware.platform.utilities.common.config.html)|Functions to support using edn files for global and environment specific configuration.|

See the [API Docs](http://nicheware.github.io/cljc-common) for complete descriptions of each namespace and each function.

Unit tests exist for every function in the library and can also be a useful source for usage examples.

## See Also

* [nicheware/clj-guidelines](http://github.com/nicheware/clj-guidelines): Clojure coding style guidelines adopted by this library.
* [funcool/Promesa](https://github.com/funcool/promesa): A promise library for Clojure and ClojureScript, used to return async values in the [config](http://nicheware.github.io/cljc-common/nicheware.platform.utilities.common.config.html) namespace.

## License

Copyright © 2018 Nicheware Solutions Pty Ltd

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version, the same as Clojure.
