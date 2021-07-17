(defproject shaderpit "0.1.0-SNAPSHOT"
  :description "Shader sandpit"
  :url "https://github.com/julesb/shaderpit"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [
                 [org.clojure/clojure "1.10.1"]
                 [quil "3.1.0"]
                 [clojure-watch "0.1.14"]
                 ]
  :main shaderpit.core
  ;:profiles {:uberjar {:aot :all}}
  :resource-paths [
      "resources/minim/minim.jar"
      "resources/minim/jsminim.jar"
      ;"resources/minim/jl1.0.1.jar"
      ;"resources/minim/mp3spi1.9.5.jar"
      ;"resources/minim/tritonus_aos.jar"
      ;"resources/minim/tritonus_share.jar"
  ]
  :jvm-opts ["-Xmx2g"]
  )
