(ns nicheware.platform.utilities.common.config-test
  (:require [nicheware.platform.utilities.common.config :as sut]
            [clojure.data :as data]
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

;; ============================ Classpath config ================================

(t/deftest test-load-config
  (t/testing "Load from test directory and merge config.edn and env-config.edn (the defaults), and resolve"
    (t/is (= {:env "test"
              :other "value"
              :file "test-file.txt"
              :value "common-value"}
             (sut/load-config "."))))

  (t/testing "Load from test directory and no resolving of variables"
    (t/is (= {:env "test"
              :other "value"
              :file "{{env}}-file.txt"
              :value "common-value"}
             (sut/load-config "." {:resolve-vars false}))))

  (t/testing "Load from dev directory and merge config.edn and env-config.edn (the defaults)"
    (let [expected-config {:version "1.2.0",
                           :company
                           {:name "Nicheware Solutions",
                            :dns "nicheware.com.au",
                            :s3-suffix "nicheware-com-au"},
                           :application {:name "PatternDesigner"},
                           :aws {:region "us-east-1"},
                           :cognito
                           {:providers [:google],
                            :google
                            {:client-id
                             "312197562856-8m63j60n1538dgak2c1bfq4d99cdi7u6.apps.googleusercontent.com"},
                            :identity-pool-id
                            "us-east-1:6f4c0aec-2488-4697-af3b-d11564da7763"},
                           :dynamodb
                           {:table-name "TestUserState",
                            :capacity {:read 3, :write 1},
                            :attributes [{:name "userId", :type "S", :key-type "HASH"}]},
                           :env "test"}]
      (t/is (= [nil nil expected-config]
               (data/diff expected-config
                          (sut/load-config "public/nicheware/patterns/webapps/pattern_designer")))))))
