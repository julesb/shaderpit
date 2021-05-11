(ns shaderpit.transport
  (:require [quil.core :as q]
            [shaderpit.console :as console]
            [shaderpit.util :as util]
            ))

;(def current-recording (atom []))

(def transport (atom nil))

(def ^:dynamic ui-font)

(def initial-transport {
  :status :idle  ; :idle :record :play
  :current-recording nil
  :current-time 0
  :current-frame-idx 0
  :loop-mode :none ; :none :one :all
})

(def new-recording {
  :name "New Recording"
  :frames []
  :shader nil
  })

(def ctx (atom {:width 300
                :height 150
                :gr nil
                :dirty true}))


(defn clean-state [state]
  (-> state
      (dissoc :aspect-ratio :keys-down :ns-delta :ns-prev
              :render-width :render-height :shaders :mousewarp)
      (update-in [:current-shader] dissoc :shaderobj)))


(defn save []
  (let [basename (get-in @transport [:current-recording :name])
        path (str "./capture/" (util/get-successor-filename basename))]
    (console/writeln (str "recorder: WRITE " path))
    (spit path (@transport :current-recording))))


(defn init []
  (reset! transport initial-transport)
 (console/writeln (str "transport: INIT: " @transport)))


(defn init-graphics [w h]
  (def ui-font (q/load-font "data/app/fonts/FreeMono-16.vlw"))
  (swap! ctx assoc :gr (q/create-graphics w h :p2d))
  
  )
  
  

(defn record [state]
  (let [basename (get-in state [:current-shader :name])
        shader (dissoc (state :current-shader) :shaderobj)
        recording (-> new-recording
                      (assoc :name basename)
                      (assoc :shader shader))
        newts (-> initial-transport
                  (assoc :current-time 0)
                  (assoc :current-frame 0)
                  (assoc :status :record)
                  (assoc :current-recording recording)) ]
    (reset! transport newts)
    (console/writeln (str "transport: RECORD"))))


(defn frame-count []
  (count (get-in @transport [:current-recording :frames])))


(defn recording? []
  (= (@transport :status) :record))
 

(defn playing? []
  (= (@transport :status) :play))
 

(defn stop []
  (swap! transport assoc :status :ready)
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
        (swap! transport update :current-frame-idx
               #(int (mod (inc %) nframes)))
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
;(defn draw-ui [x y]
; 
;  )
;
