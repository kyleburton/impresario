(defproject com.github.kyleburton/impresario "1.0.10"
  :description "Impresario: Workflow for Clojure"
  :url         "http://github.com/kyleburton/impresario"
  :license {:name "Eclipse Public License - v 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo
            :comments "same as Clojure"}
  :jvm-opts ["-Xmx512M"]
  ; :warn-on-reflection true
  :dev-dependencies [[swank-clojure "1.2.1"]
                     ;;[autodoc "0.7.1"]
                     ]
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [org.clojars.kyleburton/clj-etl-utils "1.0.32"]
                 [log4j/log4j "1.2.14"]
  ]
  :autodoc {
    :name "Impresario"
    :page-title "Impresario: API Documentation"
    :description "Workflow for Clojure"
    :web-home "http://kyleburton.github.com/projects/impresario/"
  })
