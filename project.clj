(defproject shaderpit "0.1.0-SNAPSHOT"
  :description "Shader sandpit"
  :url "https://github.com/julesb/shaderpit"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [
                 [org.clojure/clojure "1.10.1"]
                 [quil "3.1.0"]
                 ]
  :main shaderpit.core
  :profiles {:uberjar {:aot :all}}
  :jvm-opts ["-Xmx2g"]
  )
