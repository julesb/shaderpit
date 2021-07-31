(ns shaderpit.transport
  (:require [quil.core :as q]
            [shaderpit.console :as console]
            [shaderpit.util :as util]
            [clojure.java.io :as io]
            ))

;(def current-recording (atom []))

(def transport (atom nil))

(def ^:dynamic ui-font)

(def initial-transport {
  :status :idle  ; :idle :record :play
  :current-recording nil
  :current-time 0
  :current-frame-idx 0
  :loop-mode :all ; :none :one :all
  :capture-filenames []
  :capture-file-idx 0
})

(def new-recording {
  :path nil
  :name "New Recording"
  :frames []
  :shader nil
  })

(def ctx (atom {:width 300
                :height 150
                :gr nil
                :dirty true}))

(defn get-transport []
  (let [frames (get-in @transport [:current-recording :frames])
        frameplaceholder (str "["( count frames) " frames]")]
  (update @transport :current-recording assoc :frames frameplaceholder)))


(defn clean-state [state]
  (-> state
      (dissoc :aspect-ratio :keys-down :ns-delta :ns-prev
              :render-width :render-height :shaders :mousewarp)
      (update-in [:current-shader] dissoc :shaderobj)))


(defn save []
  (let [basename (get-in @transport [:current-recording :name])
        path (str "./capture/" (util/get-successor-filename basename))]
    (console/writeln (str "transport: WRITE " path))
    (spit path (@transport :current-recording))))


(defn load-capture [fname]
  (let [path (str (util/basedir :capture) "/" fname)]
    (console/writeln (str "LOAD CAPTURE: " path))
    (if (.exists (io/file path))
      (do
        (swap! transport assoc :current-recording (read-string (slurp path)))
        (swap! transport assoc :current-frame-idx 0))
      (console/writeln (str "file " path "doesn't exist")))))


(defn shader-has-captures? []
  (> (count (@transport :capture-filenames)) 0))


(defn init []
  (def ui-font (q/load-font "data/app/fonts/FreeMono-16.vlw"))
  (reset! transport initial-transport)
 (console/writeln (str "transport: INIT: " @transport)))


(defn init-graphics [w h]
  (swap! ctx assoc :gr (q/create-graphics w h :p2d)))
  
  

(defn record [state]
  (let [basename (get-in state [:current-shader :name])
        shader (dissoc (state :current-shader) :shaderobj)
        recording (-> new-recording
                      (assoc :name basename)
                      (assoc :shader shader)
                      (assoc :path (util/get-successor-filename basename)))
        newts (-> initial-transport
                  (assoc :current-time 0)
                  (assoc :current-frame 0)
                  (assoc :status :record)
                  (assoc :current-recording recording)) ]
    (reset! transport newts)
    (console/writeln (str "transport: RECORD"))))


(defn load-capture-filelist [shaderdef]
  (swap! transport assoc :capture-filenames
         (util/get-capture-files (shaderdef :name) ))
  )

(defn frame-count []
  (count (get-in @transport [:current-recording :frames])))


(defn recording? []
  (= (@transport :status) :record))
 

(defn playing? []
  (= (@transport :status) :play))
 

(defn stop []
  (swap! transport assoc :status :idle)
  (console/writeln "transport: STOP") )


(defn play []
  (swap! transport assoc :status :play)
  (console/writeln "transport: PLAY"))


(defn on-record [state]
  (if (recording?)
    (do
      (stop)
      (save))
    (record state)))


(defn on-play-pause []
  (if (playing?)
    (stop)
    (if (recording?)
      (do
        (stop)
        (save))
      (play))))


(defn on-shader-change [shaderdef]
  (load-capture-filelist shaderdef)
  (swap! transport assoc :capture-file-idx 0)
  (swap! transport assoc :current-recording new-recording)
  (when (shader-has-captures?)
    (load-capture (get-in @transport [:capture-filenames 0]))))


(defn max-frame-idx []
  (dec (frame-count)))


(defn check-loop []
  (when (> (@transport :current-frame-idx) (max-frame-idx))
    (console/writeln "check-loop frame condition met")
    (cond
      (= (@transport :loop-mode) :none)
        (stop)
      (= (@transport :loop-mode) :one)
          (swap! transport assoc :current-frame-idx 0)
      (and (= (@transport :loop-mode) :all)
           (shader-has-captures?))
        (let [files (@transport :capture-filenames)
                curr-file-idx (@transport :capture-file-idx)
                curr-filename (get-in @transport [:current-recording :path])
                num-files (count (@transport :capture-filenames))
                new-fidx (mod (inc curr-file-idx) num-files)
                fname (files new-fidx) ]
          (console/writeln (str "END " curr-file-idx))
          (swap! transport assoc :capture-file-idx new-fidx)
          (load-capture fname)))))


(defn capture-frame [state]
  (when (recording?)
    (swap! transport update-in [:current-recording :frames]
                               conj (clean-state state)))
  state)
  


(defn current-frame [state]
  (let [frames (get-in @transport [:current-recording :frames])
        fidx (@transport :current-frame-idx)
        nframes (count frames)
        shader (state :current-shader)]
    (if (> nframes 0)
      (do
        (swap! transport update :current-frame-idx inc)
        (check-loop)
        (merge state (assoc (frames fidx) :current-shader shader)))
      state)))

;
;
;
;(defn render [t]
;  (q/with-graphics (@ctx :gr)
;    (q/text-font ui-font)
;    (q/background 0 0 0 64)
;
;  ))
;
;


(defn draw-rec-icon [x y t]
  (let [blink-rate 2.0
        t (util/fract (* t blink-rate))
        alpha (* 255 (Math/pow (+ t 1.0) -4.0))]
    (q/no-stroke)
    (q/fill 255 0 0 alpha)
    (q/ellipse x y 30 30)))


(defn draw-play-icon [x y t]
  (let [alpha (+ 0.5 (* 0.5 (+ (* (Math/sin (* t 50)) 0.5) 0.5) )) ]
  ;(q/fill 0 255 0 (* alpha 255))
  (q/fill 0 128 0)
  (q/no-stroke)
  (q/with-translation [x y]
    (q/scale 15)
    (q/begin-shape :triangles)
      (q/vertex 1.0 0.0)
      (q/vertex -0.4999818309565503 0.8660358934324464)
      (q/vertex -0.5000363374266711 -0.8660044233448928)
    (q/end-shape))))


(defn draw-idle-icon [x y t]
  (let [s 10]
    (q/fill 128 128 128)
    (q/with-translation [x y]
      (q/rect (- s) (- s) (* s 2) (* s 2)))))




(defn draw-info [x y t]
  (q/text-font ui-font)
    (let [line-space 30
          cur-frame-idx (@transport :current-frame-idx)
          nframes (count (get-in @transport [:current-recording :frames]))
          name (get-in @transport [:current-recording :name])
          path (get-in @transport [:current-recording :path] "<none>")
          loopmode (@transport :loop-mode)
          status (@transport :status)
          lines [
            (str "file: " path)
            (str "frame: " cur-frame-idx " / " nframes)
            (str "status: " status)
            (str "loop: " loopmode)
            ]
          ]
      (doseq [i (range (count lines))]
        (q/fill 0 0 0 128)
        (q/rect (- x 6) (- (+ y (* i line-space)) 20)
                (+ (q/text-width (lines i)) 12) 26 12)
        (q/fill 255 255 255 192)
        (q/text (lines i) x (+ y (* i line-space))))))


(defn draw-transport [x y w h t]
  (let [cur-frame-idx (@transport :current-frame-idx)
        nframes (count (get-in @transport [:current-recording :frames]))
        ;name (get-in @transport [:current-recording :name])
        path (get-in @transport [:current-recording :path] "<none>")
        loopmode (@transport :loop-mode)
        status (@transport :status)
        status-pos [30 (/ h 2)]
        textheight 20
        path-pos [60 25]
        framecnt-pos [60 (+ (/ h 2) 20)]
        tline-x 210
        tline-y (+ (/ h 2) 12)
        tline-w 270
        tline-h 10
        prog-w (if (> nframes 0) (* (/ tline-w nframes) cur-frame-idx) 0)
        ]
    (q/text-font ui-font)
    (q/fill 0 0 0 192)
    (q/with-translation [x y]
      (q/rect 0 0 w h)
      (cond
        (= status :play)
          (draw-play-icon (status-pos 0) (status-pos 1) t)
        (= status :record)
          (draw-rec-icon (status-pos 0) (status-pos 1) t)
        :else
          (draw-idle-icon (status-pos 0) (status-pos 1) t)
          )
      (q/fill 255 255 255)
      (q/text (str path " [" (inc (@transport :capture-file-idx))
                   "/" (count (@transport :capture-filenames)) "]" )
              (path-pos 0) (path-pos 1))
      (q/text (str cur-frame-idx "/" nframes)
              (framecnt-pos 0) (framecnt-pos 1))

      ; timeline
      (q/fill 32 32 32 192)
      (q/stroke 128 128 128 128)
      (q/rect tline-x tline-y tline-w tline-h)
      (q/fill 128 128 128)
      (q/rect tline-x tline-y prog-w tline-h)
    )
  ))


(defn draw-ui [x y t]
  (let [ts-w 500
        ts-h 70]
  ;(draw-info x y t)
  ;(draw-rec-icon (- (q/width) 50) (- (q/height) 50) t)
  (draw-transport (- (/ (q/width) 2.0) (/ ts-w 2.0))
                  (- (q/height) ts-h)
                  ts-w ts-h t)
  ))

;
