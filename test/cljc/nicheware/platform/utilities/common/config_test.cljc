(ns nicheware.platform.utilities.common.config-test
  (:require [nicheware.platform.utilities.common.config :as sut]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))


;; ========================== Helper methods =====================================

(t/deftest test-resolve-template-vars
  (t/testing "Test replacement of a single variable value"
    (t/is (= {:param "param-value"
              :usage "param-value-suffix"}
             (sut/resolve-template-vars {:param "param-value"
                                         :usage "{{param}}-suffix"}))))

  (t/testing "Test replacement of a single variable value using a function"
    (t/is (= {:param "param-value"
              :usage "PARAM-VALUE-suffix"}
             (sut/resolve-template-vars {:param "param-value"
                                         :usage "{{#upper}}{{param}}{{/upper}}-suffix"}))))


  (t/testing "Test replacement of a multiple variables in nested map"
    (t/is (= {:param "my-value"
              :usage "prefix-my-value"
              :nested {:usage "my-value-suffix"}
              }
             (sut/resolve-template-vars {:param "my-value"
                                         :usage "prefix-my-value"
                                         :nested {:usage "{{param}}-suffix"}
                                         }))))

  (t/testing "Test replacement of recursive variables in nested map"
    (t/is (= {:first "one"
              :second "one-two"
              :third "one-two-three"
              }
             (sut/resolve-template-vars {:first "one"
                                         :second "{{first}}-two"
                                         :third "{{second}}-three"
                                         })))))
