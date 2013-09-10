(defproject com.github.kyleburton/impresario "1.0.12-SNAPSHOT"
  :description "Impresario: Workflow for Clojure"
  :url         "http://github.com/kyleburton/impresario"
  :lein-release {:deploy-via :clojars}
  :license {:name "Eclipse Public License - v 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo
            :comments "same as Clojure"}
  :repositories         {"sonatype-oss-public" "https://oss.sonatype.org/content/groups/public/"}
  :local-repo-classpath true
  :autodoc {
    :name "Impresario"
    :page-title "Impresario: API Documentation"
    :description "Workflow for Clojure"
    :web-home "http://kyleburton.github.com/projects/impresario/"
  }
  :profiles             {:dev {:dependencies [[swank-clojure "1.4.3"]]}
                         :1.2 {:dependencies [[org.clojure/clojure "1.2.1"]]}
                         :1.3 {:dependencies [[org.clojure/clojure "1.3.0"]]}
                         :1.4 {:dependencies [[org.clojure/clojure "1.4.0"]]}
                         :1.5 {:dependencies [[org.clojure/clojure "1.5.1"]]}
                         :1.6 {:dependencies [[org.clojure/clojure "1.6.0-master-SNAPSHOT"]]}}
  :aliases              {"all" ["with-profile" "dev,1.2:dev,1.3:dev,1.4:dev,1.5:dev,1.6"]}
  :global-vars          {*warn-on-reflection* true}
  :dependencies         [])
