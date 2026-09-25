(ns nicheware.platform.utilities.common.hello-test
  (:require-macros [cljs.test :refer (is deftest testing)])
  (:require [cljs.test]
            [nicheware.platform.utilities.common.core :as common]))

(deftest example-passing-test
  (common/debug "example-passing-test" "an example-passing-test")
  (is (= 1 1)))
