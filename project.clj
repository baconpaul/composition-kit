(defproject composition-kit "0.1.0-SNAPSHOT"
  :description "Paul's Idiosyncratic Composition Kit"
  :url "http://example.com/FIXME"
  :license {:name "Apache License 2.0"
            :url "http://www.apache.org/licenses/LICENSE-2.0"}
  :dependencies [
                 [org.clojure/clojure "1.8.0"]
                 [instaparse "1.4.3"]
                 [org.clojure/data.csv "0.1.3"]
		 [org.jfree/jfreechart "1.0.19"]
                 ]
  :jvm-opts ["-Xmx2g"]
  :source-paths ["src/"]
  :java-source-paths ["javasrc/"]
  )
