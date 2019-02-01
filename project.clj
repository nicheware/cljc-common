(defproject nicheware/cljc-common "1.0.0"
  :description "Clojure/Clojurescript library of utility functions common to service and client development"
  :url "http://github.com/nicheware/cljc-common"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha17"]
                 [org.clojure/clojurescript "1.9.562"]
                 [org.clojure/core.async "0.3.443"]
                 [org.clojure/core.match "0.3.0-alpha5"]
                 [funcool/promesa "1.9.0"]
                 [funcool/cats "2.1.0"]     ;; ensure 2.0.0 not included by below.
                 [io.nervous/kvlt "0.1.4"]
                 [cljstache "2.0.1"]
                 [thi.ng/strf "0.2.2"]
                 ]

  :source-paths ["src/cljc"]

  :test-paths ["test/cljc"]

  :plugins [[lein-codox "0.10.5"]
            [lein-doo "0.1.10"]]

  :cljsbuild {:builds
              [
               {:id "test"
                :source-paths ["src/cljc" "test/cljc" "test/cljs"]
                :compiler {:asset-path "base/resources/public/js/compiled/out"
                           :output-dir "resources/public/js/compiled/out"
                           :output-to "resources/public/js/compiled/testable.js"
                           :main nicheware.platform.utilities.common.client.test-runner
                           :recompile-dependents false
                           :language-in :ecmascript5
                           :optimizations :none}}]}

  :doo {:build "test"
        :alias {:default [:chrome]}}

  :profiles
  {:dev {:dependencies [[camel-snake-kebab "0.4.0"]
                        ]
         :source-paths ["dev"]
         :resource-paths ["env/dev/resources"]
         }

   :test {:dependencies [[ring/ring-core "1.7.1"]
                         [ring/ring-jetty-adapter "1.7.1"]
                         [doo "0.1.10"]
                         ]
          :resource-paths ["env/test/resources" "test/resources"]
          }

   :codox {:dependencies [[codox-theme-rdash "0.1.2"]
                          [nicheware/clj-codox-deeptree "0.1.0-SNAPSHOT"]]
           :plugins [[lein-codox "0.10.5"]]
           :codox {:project {:name "cljc-common"}
                   :metadata {:doc/format :markdown}
                   :themes [:rdash :deeptree]
                   :source-paths ["src/cljc"]
                   :source-uri "https://github.com/nicheware/cljc-common/blob/master/{filepath}#L{line}"
                   }}
   }
  :aliases {"codox" ["with-profile" "+codox" "codox"]}

  )
