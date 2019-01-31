(ns nicheware.platform.utilities.common.client.test-runner
  (:require
   [doo.runner :refer-macros [doo-tests]]
   [nicheware.platform.utilities.common.hello-test]
   [nicheware.platform.utilities.common.core-test]
   [nicheware.platform.utilities.common.graphics-test]
   [nicheware.platform.utilities.common.graphics.color-test]
   [nicheware.platform.utilities.common.graphics.line-test]
   [nicheware.platform.utilities.common.graphics.interpolate-test]
   [nicheware.platform.utilities.common.state.migration-test]
   [nicheware.platform.utilities.common.version-test]
   [nicheware.platform.utilities.common.math-test]
   ))

(enable-console-print!)

(doo-tests 'nicheware.platform.utilities.common.hello-test
           'nicheware.platform.utilities.common.core-test
           'nicheware.platform.utilities.common.graphics-test
           'nicheware.platform.utilities.common.graphics.color-test
           'nicheware.platform.utilities.common.graphics.line-test
           'nicheware.platform.utilities.common.graphics.interpolate-test
           'nicheware.platform.utilities.common.state.migration-test
           'nicheware.platform.utilities.common.version-test
           'nicheware.platform.utilities.common.math-test
           )
