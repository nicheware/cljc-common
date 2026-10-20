(ns nicheware.platform.utilities.common.string-test
  (:require [nicheware.platform.utilities.common.string :as sut]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

(defn illegal-arg?
  "True if calling f throws the platform's illegal argument exception."
  [f]
  (try
    (f)
    false
    (catch #?(:clj IllegalArgumentException :cljs js/Error) ex
      #?(:clj true
         :cljs (boolean (re-find #"IllegalArgumentException" (ex-message ex)))))))

;; ============================== Markup escaping ===============================

(t/deftest test-escape-html
  (t/testing "Special characters escaped"
    (t/are [expected s] (= expected (sut/escape-html s))
      "Fish &amp; Chips &lt;b&gt;" "Fish & Chips <b>"
      "&amp;amp;"                  "&amp;"
      "&lt;&lt;&gt;&gt;"           "<<>>"
      "say \"hi\" 'bye'"           "say \"hi\" 'bye'"
      ""                           ""
      nil                          nil))

  (t/testing "Unchanged input returned as the identical string"
    (let [s "plain text, no specials"]
      (t/is (identical? s (sut/escape-html s)))))

  (t/testing "Invalid arguments"
    (t/is (illegal-arg? #(sut/escape-html 42)))
    (t/is (illegal-arg? #(sut/escape-html :a)))))

(t/deftest test-escape-attr
  (t/testing "Special characters escaped"
    (t/are [expected s] (= expected (sut/escape-attr s))
      "say &quot;hi&quot; &amp; &#39;bye&#39;" "say \"hi\" & 'bye'"
      "a&lt;b&gt;c"                            "a<b>c"
      "&amp;quot;"                             "&quot;"
      ""                                       ""
      nil                                      nil))

  (t/testing "Unchanged input returned as the identical string"
    (let [s "plain-value_1"]
      (t/is (identical? s (sut/escape-attr s)))))

  (t/testing "Invalid arguments"
    (t/is (illegal-arg? #(sut/escape-attr 42)))))
