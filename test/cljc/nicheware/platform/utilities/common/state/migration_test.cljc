(ns nicheware.platform.utilities.common.state.migration-test
  (:require  [nicheware.platform.utilities.common.state.migration :as sut]
             #?(:clj [clojure.test :as t]
                :cljs [cljs.test :as t :include-macros true])))

;; ================================== Data used in tests ==============================

(defn test-migration-fn
  "Test function that will change :data from a string to a vector of that string"
  [{:keys [data] :as  state}]
  (assoc state :data [data]))

(defn test-migration-fn2
  "Test function that will add extra element to data vector"
  [{:keys [data] :as  state}]
  (assoc state :data (cons "1" data)))

(def test-state
  {:version "1.0.4"
   :data "string-value"})

(def test-migrations
  {"1.0" {:from-version "1.0"
          :to-version "1.1.3"
          :migration-fns [test-migration-fn identity]}
   "1.1" {:from-version "1.1"
          :to-version "2.3.0"
          :migration-fns [test-migration-fn2]}})

;; ================================= Implementation helper functions ==================

(t/deftest test-major-version
  (t/testing "Test we can get the major version from a version string"
    (t/is (= 1
             (sut/major-version "1.2.3")))))

(t/deftest test-minor-version
  (t/testing "Test we can get the minor version from a version string"
    (t/is (= 2
             (sut/minor-version "1.2.3")))))

(t/deftest test-major-minor-version
  (t/testing "Test we can get the major minor version from a version string with a patch"
    (t/is (= "1.2"
             (sut/major-minor-version "1.2.3"))))

  (t/testing "Test we can get the major minor version from a version string without a patch"
    (t/is (= "1.2"
             (sut/major-minor-version "1.2")))))


;; ================================= Interface functions ===============================

(t/deftest test-could-upgrade-version
  (t/testing "Test where version lower in major"
    (t/is (sut/could-upgrade-version "1.2.5" "2.0.0")))

  (t/testing "Test where version lower in minor"
    (t/is (sut/could-upgrade-version "1.2.5" "1.3.0")))

  (t/testing "Test where major and minor the same"
    (t/is (sut/could-upgrade-version "1.2.5" "1.2.0")))

  (t/testing "Test where cant upgrade in major"
    (t/is (not  (sut/could-upgrade-version "2.2.5" "1.2.0"))))

  (t/testing "Test where cant upgrade in minor"
    (t/is (not (sut/could-upgrade-version "2.3.5" "2.2.0")))))

(t/deftest test-migrate-state
  (let [new-state (sut/migrate-state test-state "1.1.4" test-migrations)]
    (t/testing "test simple migration will update version number correctly"
      (t/is (= "1.1.3"
               (:version new-state))))

    (t/testing "test simple migration will update state as required"
      (t/is (= ["string-value"]
               (:data new-state)))))

  (let [new-state (sut/migrate-state test-state "2.3.3" test-migrations)]
    (t/testing "test simple migration will update version number correctly"
      (t/is (= "2.3.0"
               (:version new-state))))

    (t/testing "test simple migration will update state as required"
      (t/is (= ["1" "string-value"]
               (:data new-state)))))

  (let [new-state (sut/migrate-state test-state "1.2.0" test-migrations)]
    (t/testing "test migration will stop correctly"
      (t/is (= "1.1.3"
               (:version new-state))))

    (t/testing "test migration stopping early will update state as required"
      (t/is (= ["string-value"]
               (:data new-state))))))
