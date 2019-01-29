(ns nicheware.platform.utilities.common.version-test
  (:require [nicheware.platform.utilities.common.version :as sut]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])
            [nicheware.platform.utilities.common.core :as common]))


;; ================================= Version reference functions ==============================

(def test-ref-vec [{:val 1 :ref ["one" 1]}
                   {:val 2 :ref ["two" 2]}
                   {:val 3 :ref ["three" 3]}])

(t/deftest test-make-ref-name-fn
  (let [test-fn (sut/make-ref-name-fn :ref "match")]
    (t/testing "Test that returned function is true as expected"
      (t/is (test-fn {:ref ["match" 1]})))

    (t/testing "Test that returned function is false as expected"
      (t/is (not (test-fn {:ref ["no-match" 1]}))))))

(t/deftest test-find-element-with-ref-name
  (t/testing "Finds an element with the provided name regardless of the version."
    (t/is (= {:val 2 :ref ["two" 2]}
             (sut/find-element-with-ref-name test-ref-vec :ref "two")))))

(t/deftest test-replace-element-with-ref-name
  (t/testing "Test updating the second element in the vec"
    (t/is (= [{:val 1 :ref ["one" 1]}
              {:val 4 :ref ["two" 4]}
              {:val 3 :ref ["three" 3]}]
             (sut/replace-element-with-ref-name test-ref-vec {:val 4 :ref ["two" 4]} :ref "two")))))

;; ============================= Versioned asset functions =========================================


(def test-versioned-asset {:current 1
                           :name "Simple"
                           :versions {1 {:name "Simple" :value 1 :modified-time 1}
                                      2 {:name "Simple" :value 2 :modified-time 2}
                                      }})

(def test-assets {"Simple" test-versioned-asset
                  "Next" {:current 10 :name "Next" :versions {10 {:name "Next"}} }})

(t/deftest test-get-ordered-versions
  (t/testing "Just gets the versions in modified order"
    (t/is (= [{:name "Simple" :value 1 :modified-time 1}
              {:name "Simple" :value 2 :modified-time 2}]
             (sut/get-ordered-versions test-versioned-asset)))))


(t/deftest test-find-index
  (t/testing "FInd the index of the second in list"
    (t/is (= 1
             (sut/find-index test-versioned-asset {:modified-time 2 :name "Simple"})))))


(t/deftest test-current-version-key
  (t/testing "Test we get the correct current key"
    (t/is (= 1
             (sut/current-version-key test-assets "Simple"))))

  (t/testing "Test we get nil for unknown asset"
    (t/is (nil?
             (sut/current-version-key test-assets "Unknown")))))


(t/deftest test-get-current-ref
  (t/testing "Test we get a valid reference to the asset"
    (t/is (= ["Simple" 1]
             (sut/get-current-ref test-assets "Simple"))))

  (t/testing "Test we get nil in reference if no asset"
    (t/is (= ["Unknown" nil]
             (sut/get-current-ref test-assets "Unknown")))))


(t/deftest test-get-first-asset-ref
  (t/testing "Gets the reference to the first asset in the map"
    (t/is (= ["Simple" 1]
             (sut/get-first-asset-ref test-assets)))))

(t/deftest test-get-version
  (t/testing "Gets a specific version of an asset"
    (t/is (= {:name "Simple" :value 2 :modified-time 2}
             (sut/get-version test-assets ["Simple" 2])))))

(t/deftest test-get-current
  (t/testing "Test we can get the current version of an asset"
    (t/is (= {:name "Simple" :value 1 :modified-time 1}
             (sut/get-current test-assets "Simple")))))

(t/deftest test-get-current-asset
  (t/testing "Get current version from a versioned asset"
    (t/is (= {:name "Simple" :value 1 :modified-time 1}
             (sut/get-current-asset test-versioned-asset)))))

(t/deftest test-rename-versions
  (t/testing "Renames all the entries in the versions map"
    (t/is (= {1000 {:name "New" :modified-time 1000}
              2000 {:name "New" :modified-time 2000}}
             (sut/rename-versions {1000 {:name "Old" :modified-time 1000}
                                   2000 {:name "Old" :modified-time 2000}} "New")))))

(t/deftest test-rename-versioned-asset
  (t/testing ""
    (t/is (= {:current 1
              :name "NewName"
              :versions {1 {:name "NewName" :value 1 :modified-time 1}
                         2 {:name "NewName" :value 2 :modified-time 2}
                         }}
             (sut/rename-versioned-asset test-versioned-asset "NewName")))))


(t/deftest test-version-element
  (t/testing "Add timestamp and set starred false"
    (let [versioned (sut/version-element {:value 1 :starred true})]
      (t/is (= false (:starred versioned)))
      (t/is (not (nil? (:modified-time versioned)))))))


(t/deftest test-timestamp-element
  (t/testing "Check when not set"
    (t/is (<= (common/current-time-millis) (:modified-time (sut/timestamp-element {:key 1})))))

  (t/testing "Check when set"
    (t/is (= 203 (:modified-time (sut/timestamp-element {:key 1 :modified-time 203}))))))
(t/deftest test-add-version
  (t/testing "Test we can add a new version to a sample versioned asset"
    (let [{:keys [current] :as updated} (sut/add-version test-versioned-asset {:name "Simple" :value 3 :modified-time 10})]
      (t/is (= {:current current
                :name "Simple",
                :versions
                {1 {:name "Simple", :value 1 :modified-time 1},
                 2 {:name "Simple" :value 2 :modified-time 2},
                 current {:name "Simple", :value 3, :modified-time current, :starred false}}}
               updated)))))


(t/deftest test-replace-current
  (t/testing "Replace value in current"
    (t/is (= {:current 1
              :name "Simple"
              :versions {1 {:name "Simple" :value 100 :modified-time 1}
                         2 {:name "Simple" :value 2 :modified-time 2}
                         }}
             (sut/replace-current test-versioned-asset {:name "Simple" :value 100 :modified-time 1})))))

(t/deftest test-set-version
  (t/testing "Set the version to an exiting one in the asset"
    (t/is (= 2
             (:current (sut/set-version test-versioned-asset {:value 2 :modified-time 2}))))))

(t/deftest test-mutate-version
  (t/testing "Add new version with mutation that doesn't exist"
    (let [updated-versioned-asset (sut/mutate-version 24 test-versioned-asset {:name "Simple" :value 22})
          updated-current (sut/get-current-asset updated-versioned-asset)]
      (t/is (= (:value updated-current) 22))
      (t/is (= (:value (get (:versions updated-versioned-asset) 1)) 1))

      (t/testing "Update version that has the same mutation"
        (let [next-versioned-asset (sut/mutate-version 24 updated-versioned-asset {:name "Simple" :value 34})
              next-current (sut/get-current-asset next-versioned-asset)]
          (t/is (= (:value next-current) 34))
          (t/is (= (:modified-time next-current) (:modified-time updated-current))))))))

(t/deftest test-delete-version
  (t/testing "Delete a version which exists, leaving current as is"
    (t/is (= {:current 1
              :name "Simple"
              :versions {1 {:name "Simple" :value 1 :modified-time 1}
                         }}
             (sut/delete-version test-versioned-asset 2))))

  (t/testing "Delete a version which exists, updating current"
    (t/is (= {:current 2
              :name "Simple"
              :versions {2 {:name "Simple" :value 2 :modified-time 2}
                         }}
             (sut/delete-version test-versioned-asset 1)))))

(t/deftest test-delete-asset-version
  (t/testing "Delete a version which exists, leaving current as is"
    (t/is (= {:current 1
              :name "Simple"
              :versions {1 {:name "Simple" :value 1 :modified-time 1}
                         }}
             (sut/delete-asset-version test-versioned-asset {:name "Simple" :value 2 :modified-time 2}))))

  (t/testing "Delete a version which exists, updating current"
    (t/is (= {:current 2
              :name "Simple"
              :versions {2 {:name "Simple" :value 2 :modified-time 2}
                         }}
             (sut/delete-asset-version test-versioned-asset {:name "Simple" :value 1 :modified-time 1})))))

(t/deftest test-remove-unused-versions
  (t/testing "Removes a version leaving the current and a starred one"
    (t/is (= {:current 1
              :name "Simple"
              :versions {1 {:name "Simple" :value 1 :modified-time 1}
                         3 {:name "Simple" :value 2 :modified-time 3 :starred true}
                         }}
             (sut/remove-unused-versions {:current 1
                                          :name "Simple"
                                          :versions {1 {:name "Simple" :value 1 :modified-time 1}
                                                     2 {:name "Simple" :value 2 :modified-time 2}
                                                     3 {:name "Simple" :value 2 :modified-time 3 :starred true}
                                                     }})))))
