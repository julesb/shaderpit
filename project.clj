(defproject shaderpit "0.1.0-SNAPSHOT"
  :description "Shader sandpit"
  :url "https://github.com/julesb/shaderpit"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [
                 [org.clojure/clojure "1.10.1"]
                 [quil "3.1.0"]
                 [clojure-watch "0.1.14"]
                 [com.jsyn/jsyn "20170815"]
                 ]
  :main shaderpit.core
  ;:profiles {:uberjar {:aot :all}}
  :resource-paths ["resources/processing/sound/sound.jar"]
  :jvm-opts ["-Xmx2g"]
  )
