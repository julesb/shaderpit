(ns shaderpit.util
  (:require [clojure.string :as str]
            [shaderpit.console :as console]
            [quil.core :as q :only [load-shader width height]]
            [clojure.java.io :as io]
            ))

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
  ;:id 0
  :name "Unnamed"
  :path ""
  :type :fragment
  :shaderobj nil
  })

(def initial-camera {
  :pos [5.0 1.0 0.0]
  :az 0.0
  :alt 0.0
  :az-vel 0.0
  :alt-vel 0.0
  :lookat [0.0 0.0 0.0]
  :vpn [0.0 0.0 -1.0]
  :fov (/ Math/PI 3.0)
  :vel [0.0 0.0 0.0]
  :speed 1.0
  :damp-r 0.0005
  :damp-m 0.5
  :zoom 1.0
})

(def initial-params {
  :blend_coef 10.0
  :ray_hit_epsilon 0.001
  :palette_offset 0.0
  :gamma 0.55
  :glow-intensity 0.0
  :diff-spec 0.5
})

(def initial-state {
  :keys-down #{}
  :mouse-position [0 0]
  ;:mouse-delta [0 0]
  :mousewarp true
  :render-width 0
  :render-height 0
  :aspect-ratio 1.0
  :camera-model :3d ; :3d = 3d first person, :2d = 2d pan and zoom
  :pixel-scale (int 2)
  :render-paused? false
  :camera initial-camera
  :params initial-params
  :shaders {}
  :current-shader nil
  :current-shader-idx 0
  :ns-now 0
  :ns-delta 0
  :ns-prev 0
})

(defn ns-time []
  (System/nanoTime))


(defn clock-reset [state]
  (-> state
      (assoc :ns-prev (ns-time))
      (assoc :ns-now 0)
      (assoc :ns-delta 0)))


(defn clock-tick [state]
  (let [ns-now (ns-time)
        ns-delta (- ns-now (state :ns-prev))]
    (-> state
        (assoc :ns-prev ns-now)
        (update-in [:ns-now] + ns-delta)
        (assoc :ns-delta ns-delta))))


(defn t-delta [state]
  (double (/ (state :ns-delta) 1000000000)))


(defn t-now [state]
  (double (/ (state :ns-now) 1000000000)))



(defn define-shader
  [& {:keys [id name path type shaderobj]
      :or {id (default-shader :id)
           name (default-shader :path) ; name=path
           path (default-shader :path)
           type (default-shader :type)
           shaderobj (default-shader :shaderobj) } }]
  (-> default-shader
      (assoc :id id)
      (assoc :name name)
      (assoc :path path)
      (assoc :type type)
      (assoc :shaderobj shaderobj) ))


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


(defn new-state []
  (let [shaderlist (load-shader-dir)
        current-shader (first shaderlist)
        shaderobj (q/load-shader (current-shader :path))
        render-width (int (/ (q/width) 2))
        render-height (int (/ (q/height) 2))]
    (-> initial-state
        (assoc :render-width render-width)
        (assoc :render-height render-height)
        (assoc :aspect-ratio (/ (float (q/width)) (q/height)))
        (assoc :shaders shaderlist)
        (assoc :current-shader (assoc current-shader :shaderobj shaderobj))
        (clock-reset))))


(defn load-shader-state [state shaderdef]
  (let [basename (shaderdef :name)
        statepath (str "./state/" basename ".state")]
    (if (.exists (io/file statepath))
      (let [newstatestr (slurp statepath)
            newstate (read-string newstatestr)]
        (console/writeln (str "load state: " statepath))
        (-> state
            (merge newstate)
            (assoc :ns-prev (System/nanoTime))
            (assoc :current-shader shaderdef)))
      state)))


(defn init-with-shaderpath [shaderpath]
  (console/writeln (str "init from path:" shaderpath))
  (let [shaderlist (load-shader-dir)
        shaderobj (q/load-shader shaderpath)
        shaderdef (define-shader
                    :id (default-shader :id)
                    :path shaderpath
                    :name (shader-basename shaderpath)
                    :shaderobj shaderobj)
        savedstate (load-shader-state initial-state shaderdef)
        render-width (int (/ (q/width) 2))
        render-height (int (/ (q/height) 2))]
    (-> savedstate
        (assoc :render-width render-width)
        (assoc :render-height render-height)
        (assoc :aspect-ratio (/ (float (q/width)) (q/height)))
        (assoc :shaders shaderlist)
        (clock-reset))))


(defn init-with-shaderdef [shaderdef]
  (console/writeln (str "init from def: " shaderdef))
  (let [shaderobj (q/load-shader (shaderdef :path))
        savedstate (load-shader-state initial-state shaderdef)
        render-width (int (/ (q/width) 2))
        render-height (int (/ (q/height) 2))]
    (-> savedstate
        (assoc :render-width render-width)
        (assoc :render-height render-height)
        (assoc :aspect-ratio (/ (float (q/width)) (q/height)))
        (update :current-shader assoc :shaderobj shaderobj)
        (assoc :shaders (load-shader-dir))
        (clock-reset))))


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



