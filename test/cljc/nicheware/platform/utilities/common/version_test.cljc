(ns nicheware.platform.utilities.common.version-test
  (:require [nicheware.platform.utilities.common.version :as sut]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))


;; ================================= Version reference functions ==============================

(def test-ref-vec [{:val 1 :ref ["one" 1]}
                   {:val 2 :ref ["two" 2]}
                   {:val 3 :ref ["three" 3]}])

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

(t/deftest test-get-ordered-versions
  (t/testing "Just gets the versions in modified order"
    (t/is (= [{:name "Simple" :value 1 :modified-time 1}
              {:name "Simple" :value 2 :modified-time 2}]
             (sut/get-ordered-versions test-versioned-asset)))))


(t/deftest test-find-index
  (t/testing "FInd the index of the second in list"
    (t/is (= 1
             (sut/find-index test-versioned-asset {:modified-time 2 :name "Simple"})))))

(t/deftest test-rename-versioned-asset
  (t/testing ""
    (t/is (= {:current 1
              :name "NewName"
              :versions {1 {:name "NewName" :value 1 :modified-time 1}
                         2 {:name "NewName" :value 2 :modified-time 2}
                         }}
             (sut/rename-versioned-asset test-versioned-asset "NewName")))))



(t/deftest test-add-version
  (t/testing "Test we can add a new version to a sample versioned asset"
    (let [{:keys [current] :as updated} (sut/add-version test-versioned-asset {:name "Simple" :value 3 :modified-time 1})]
      (t/is (= {:current current
                :name "Simple",
                :versions
                {1 {:name "Simple", :value 1 :modified-time 1},
                 2 {:name "Simple" :value 2 :modified-time 2},
                 current {:name "Simple", :value 3, :modified-time current, :starred false}}}
               updated)))))


(t/deftest test-set-version
  (t/testing "Set the version to an exiting one in the asset"
    (t/is (= 2
             (:current (sut/set-version test-versioned-asset {:value 2 :modified-time 2}))))))

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
