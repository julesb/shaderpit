(ns shaderpit.util
  (:require [clojure.string :as str]
            [shaderpit.console :as console]))

(def ^:const filepattern {
  :glsl #".+\.glsl$"
  :state #".+\.state$"
  :capture #".+\.capture$"
  :all #".+"
  })

(def ^:const basedir {
  :glsl "./glsl"
  :state "./state"
  :capture "./capture"
  })

(def default-shader {
  :id 0
  :name "Unnamed"
  :path ""
  :type :fragment
  :shaderobj nil
  })



(defn define-shader
  [& {:keys [id name path type]
      :or {id (default-shader :id)
           name (default-shader :path) ; name=path
           path (default-shader :path)
           type (default-shader :type) } }]
  (-> default-shader
      (assoc :id id)
      (assoc :name name)
      (assoc :path path)
      (assoc :type type) ))


(defn shader-basename [shaderpath]
  (-> shaderpath
      (str/replace #"^\./glsl/" "")
      (str/replace #"\.glsl$" "")))


(defn glsl-file-list []
  (->> (seq (.list (clojure.java.io/file "./glsl")))
       (filter #(re-matches #".+\.glsl$" %))))


(defn read-dir [dir patternkey]
  ;(console/writeln (str "read dir: " dir ", patternkey: " patternkey))
  (->> (seq (.list (clojure.java.io/file dir)))
       (filter #(re-matches (filepattern patternkey) %))))


(defn load-shader-dir []
  (into []
    (map-indexed #(define-shader :id %1
                                 :path (str "./glsl/" %2)
                                 :name (shader-basename %2))
                 (glsl-file-list))))


(defn get-capture-files [basename]
  (let [re (re-pattern (str "^" basename "_[\\d]{6}.capture"))
        allfiles (read-dir (basedir :capture) :all)
        files (filter #(re-matches re %) allfiles) ]
    (sort files)))


(defn get-successor-filename [basename]
  (let [files (get-capture-files basename)]
    (if (zero? (count files))
      (str basename "_000000.capture")
      (let [lfile (last files)
            ordstr (clojure.string/replace lfile #"^.+([\d]{4})\.capture$" #(%1 1))
            ord (Integer/parseInt ordstr)
            succ (inc ord)]
        (format "%s_%04d.capture" basename succ)))))

(defn fract [f]
  (- f (Math/floor f)))
