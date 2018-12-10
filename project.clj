(defproject nicheware/cljc-common "0.1.0-SNAPSHOT"
  :description "Clojure/Clojurescript library of utility functions common to service and client development"
  :url "http://github.com/nicheware/cljc-common"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha17"]
                 [org.clojure/core.async "0.3.443"]
                 [org.clojure/core.match "0.3.0-alpha5"]
                 [funcool/promesa "1.9.0"]
                 [funcool/cats "2.1.0"]     ;; ensure 2.0.0 not included by below.
                 [io.nervous/kvlt "0.1.4"]
                 [prismatic/plumbing "0.5.3"]
                 [cljstache "2.0.1"]
                 [thi.ng/strf "0.2.2"]
                 ]

  :source-paths ["src/cljc"]

  :test-paths ["test/cljc"]

  :plugins [[lein-codox "0.10.5"]]

  :profiles
  {:codox {:dependencies [[codox-theme-rdash "0.1.2"]]
           :plugins [[lein-codox "0.10.5"]]
           :codox {:project {:name "cljc-common"}
                   :metadata {:doc/format :markdown}
                   :themes [:rdash]
                   :source-paths ["src/cljc"]
                   :source-uri "https://github.com/nicheware/cljc-common/blob/master/{filepath}#L{line}"
                   }}
   :test {:dependencies [[camel-snake-kebab "0.4.0"]]
          :resource-paths ["env/test/resources" "test/resources"]
          }
   }
  :aliases {"codox" ["with-profile" "+codox" "codox"]}

  )
