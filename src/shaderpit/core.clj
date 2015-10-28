(ns shaderpit.core
  (:require [quil.core :as q]
            [quil.middleware :as m])
  (:use [shaderpit.vector])
  (:import java.awt.event.KeyEvent)
  )

(def ^:dynamic console-font)
(def tex1 (atom nil))
(def tex2 (atom nil))
(def ^:const PI Math/PI)

; =========================================================================

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
      (assoc :name path)
      (assoc :path path)
      (assoc :type type) ))


(defn glsl-file-list []
  (->> (seq (.list (clojure.java.io/file "./glsl")))
       (filter #(re-matches #".+\.glsl$" %))))


(defn load-shader-dir []
  (into []
  (map-indexed #(define-shader :id %1
                               :path (str "./glsl/" %2)
                               :name %2)
               (glsl-file-list)))
)

(defn next-shader [state]
  (let [id (get-in state [:current-shader :id])
        newid  (mod (inc id) (count (state :shaders)))
        newshader ((get state :shaders) newid)
        newshader-obj (q/load-shader (get newshader :path)) ]
    (-> state
        (assoc :current-shader newshader)
        (assoc-in [:current-shader :shaderobj] newshader-obj)
      )
  ))

(def initial-camera {
  :pos [128.0 50.0 0.0]
  :lookat [0.0 0.0 0.0]
  :vpn [0.0 0.0 -1.0]
  :fov (/ Math/PI 3.0)
  :speed 0.5
})

(def initial-params {
  :blend_coef 20.0
  :ray_hit_epsilon 0.01
  :palette_offset 0.0
  :gamma 0.55
  :glow-intensity 0.0
})

(def initial-state {
  :keys-down #{}
  :mouse-position [0 0]
  :mousewarp true
  :aspect-ratio 1.0
  :render-paused? false
  :camera initial-camera
  :params initial-params
  :shaders {}
  :current-shader nil
})




(defn setup []
  (let [shaderlist (load-shader-dir)
        current-shader (first shaderlist)
        shaderobj (q/load-shader (current-shader :path))  ]
    ;(q/smooth)
    (q/texture-mode :normal)
    (q/texture-wrap :repeat)
    (q/noise-detail 2)
    (q/hint :disable-depth-test)
    ;(frame-rate 120)
    (def console-font (q/load-font "data/FreeMono-16.vlw"))
    (reset! tex1 (q/load-image "testpattern4po6.png"))
    ;(reset! tex1 (q/load-image "UV_Grid_Sm.jpg"))
    ;(reset! tex1 (q/load-image "uv_checker_large.png"))
    ;(reset! tex1 (q/load-image "Sky02.jpg"))
    ;(reset! tex1 (q/load-image "Sky02-blur128x12.jpg"))
    ;(reset! tex1 (q/load-image "North_South_Panorama_Equirect_360x180.jpg"))
    ;(reset! tex1 (q/load-image "QueensPark.m.jpg"))
    ;(reset! tex1 (q/load-image "beach-hdr-blur128.jpg"))
    ;(reset! tex1 (q/load-image "cubesphere.jpg"))
    ;(reset! tex1 (q/load-image "stereographic.jpg"))
    ;(reset! tex1 (q/load-image "cave_texture_01-512x512.png"))
    ;(reset! tex1 (q/load-image "seamless-black-wall-texture-decorating-inspiration-1.jpg"))
    ;(reset! tex1 (q/load-image "beach-hdr.jpg"))
    ;(reset! tex1 (q/load-image "studio010.jpg"))
    
    ;(reset! texture-shader (load-shader "data/texfrag.glsl" "data/texvert.glsl"))

    (-> initial-state
         (assoc :aspect-ratio (/ (float (q/width)) (q/height)))
         (assoc :shaders shaderlist)
         (assoc :current-shader (assoc current-shader :shaderobj shaderobj))

         )
  ))


(defn update-uniforms! [state shader] 
  (when state
    (let [mp (get state :mouse-position [0 0])
          ar (state :aspect-ratio)
          cam-pos (get-in state [:camera :pos])
          cam-lookat (get-in state [:camera :lookat])
          cam-fov (get-in state [:camera :fov])
          ]
      (.set shader "framecount" (float (q/frame-count)))
      (.set shader "aspect_ratio" (float ar))
      ;(when (mouse-pressed?)
        (.set shader "mousex" (float (mp 0)))
        (.set shader "mousey" (float (mp 1)))
      ;  )
      (.set shader "swidth" (float (q/width)))
      (.set shader "sheight" (float (q/height)))
      (.set shader "cam_pos" (float (cam-pos 0)) 
                             (float (cam-pos 1))
                             (float (cam-pos 2)))
      (.set shader "cam_lookat" (float (cam-lookat 0)) 
                                (float (cam-lookat 1))
                                (float (cam-lookat 2)))
      (.set shader "cam_fov" (float cam-fov))
      (.set shader "blend_coef" (float (get-in state [:params :blend_coef])))
      (.set shader "ray_hit_epsilon" (float (get-in state [:params :ray_hit_epsilon] 0.001)))
      (.set shader "palette_offset" (float (get-in state [:params :palette_offset] 0.0)))
      (.set shader "gamma" (float (get-in state [:params :gamma] 0.5)))
      (.set shader "glow_intensity" (float (get-in state [:params :glow-intensity] 1.0)))
      (.set shader "time" (float (q/millis)))

      )
    state))


;(def speed 0.025)
;(def turbospeed 1.5)

(defn camera-mouse-update [state]
  (let [[mx my] (vec2-mul (vec2-sub (state :mouse-position) [0.5 0.5])
                          [(* PI 2.0) (* PI 0.99)])
        pos (get-in state [:camera :pos] [0.0 0.0 0.0])
        vpn (vec3-normalize [(* (Math/cos my) (Math/cos mx))
                             (Math/sin my)
                             (* (Math/cos my) (Math/sin mx))])
        lookat (vec3-add pos (vec3-scale vpn 6.0))
        new-cam (-> (state :camera)
                    (assoc :vpn vpn)
                    (assoc :lookat lookat)) ]
  (-> state
      (assoc :camera new-cam))))


(defn do-key-movement [state keychar]
  (let [pos-old  (get-in state [:camera :pos] [0.0 0.0 0.0])
        wup [0.0 -1.0 0.0] ; world up
        vpn (get-in state [:camera :vpn])
        vpv (vec3-normalize (vec3-cross (get-in state [:camera :vpn]) [0.0 -1.0 0.0]))
        speed (get-in state [:camera :speed])
        ;vpu (vec3-normalize (vec3-cross vpn vpv)) ;viewplane up
        key-movement-map {
          \w (fn [s] (assoc-in s [:camera :pos] (vec3-add pos-old (vec3-scale vpn speed))))
          \s (fn [s] (assoc-in s [:camera :pos] (vec3-sub pos-old (vec3-scale vpn speed))))
          \a (fn [s] (assoc-in s [:camera :pos] (vec3-add pos-old (vec3-scale vpv speed))))
          \d (fn [s] (assoc-in s [:camera :pos] (vec3-sub pos-old (vec3-scale vpv speed))))
          \e (fn [s] (assoc-in s [:camera :pos] (vec3-sub pos-old (vec3-scale wup speed))))
          \c (fn [s] (assoc-in s [:camera :pos] (vec3-add pos-old (vec3-scale wup speed))))
          \b (fn [s] (update-in s [:params :blend_coef] #(- % 0.1)))
          \n (fn [s] (update-in s [:params :blend_coef] #(+ % 0.1)))
          \1 (fn [s] (update-in s [:camera :speed] #(* % 0.9)))
          \2 (fn [s] (update-in s [:camera :speed] #(/ % 0.9)))
          \- (fn [s] (update-in s [:camera :fov] #(* % 0.9)))
          \= (fn [s] (update-in s [:camera :fov] #(/ % 0.9)))
          \[ (fn [s] (update-in s [:params :ray_hit_epsilon] #(* % 0.9)))
          \] (fn [s] (update-in s [:params :ray_hit_epsilon] #(/ % 0.9)))
          \3 (fn [s] (update-in s [:params :palette_offset] #(- % 0.05)))
          \4 (fn [s] (update-in s [:params :palette_offset] #(+ % 0.05)))
          \5 (fn [s] (update-in s [:params :gamma] #(- % 0.01)))
          \6 (fn [s] (update-in s [:params :gamma] #(+ % 0.01)))
          \7 (fn [s] (update-in s [:params :glow-intensity] #(- % 0.1)))
          \8 (fn [s] (update-in s [:params :glow-intensity] #(+ % 0.1)))
         }]
  (if (contains? key-movement-map keychar)
    (-> ((key-movement-map keychar) state)
        (camera-mouse-update))
    state)))


(defn do-movement-keys [state & keys-down]
  (if (nil? keys-down)
    (recur state (state :keys-down))
    (if (<= (count keys-down) 0)
      state
      (recur (do-key-movement state (first keys-down))
             (rest keys-down)))))


(defn key-pressed [state event]
  (let [the-raw-key (event :raw-key)
        the-key-code (event :key-code)
        coded? (= processing.core.PConstants/CODED (int the-raw-key))
        the-key-pressed (if coded? the-key-code the-raw-key) ]
    (if coded?
      state
      (-> state
          (assoc :keys-down (conj (state :keys-down) the-raw-key))))))
      

(defn key-released [state]
  (-> state
      (assoc :keys-down (disj (state :keys-down) (q/raw-key)))))


(defn key-typed [state event]
  (case (event :raw-key)
    \p (do
         (if (state :render-paused?)
           (q/start-loop)
           (q/no-loop))
        (update-in state [:render-paused?] not))
    \# (do
         (q/save-frame)
         state)
    \R (do (-> initial-state
               (assoc :aspect-ratio (/ (float (q/width)) (q/height)))))
    \0 (do (-> initial-state
               (assoc :aspect-ratio (/ (float (q/width)) (q/height)))
               (assoc-in [:camera :pos] [0.0 0.0 0.0])))
    \m (do (-> initial-state
               (update-in [:mousewarp] not)))
    \` (do
         state)
    \/ (do (-> state
               (next-shader)))
    state))


(defn mouse-moved [state event]
    (-> state
        (assoc :mouse-position [(/ (event :x) (q/width))
                                (/ (event :y) (q/height))])
        (camera-mouse-update)
        ))


(defn mouse-dragged [state event]
    (-> state
        (assoc :mouse-position [(event :x) (event :y)])))




(defn update [state]
  (-> state
      (do-movement-keys)
      (update-uniforms! (get-in state [:current-shader :shaderobj]))
  ))


(defn draw-info [state x y]
  (q/text-font console-font)
  (let [line-space 24
        ar (get state :aspect-ratio 0.0)
        [mx my] (state :mouse-position)
        zoom (get state :zoom 0.0)
        pos (get-in state [:camera :pos] [0.0 0.0 0.0])
        speed (get-in state [:camera :speed] 0.0)
        fov (get-in state [:camera :fov] 0.0)
        fovdeg (/ (* fov 180.0) Math/PI)
        blend (get-in state [:params :blend_coef] 0.0)
        eps (get-in state [:params :ray_hit_epsilon] 0.0)
        gamma (get-in state [:params :gamma] 0.0)
        glow (get-in state [:params :glow-intensity] 0.0)
        shadername (get-in state [:current-shader :name] "-")
        lines [
               ;(str "state: " state)
               (str "shader: " shadername)
               (str "pos: " (vec3-format pos))
               (str (format "mouse: [%.2f %.2f]" (float mx) (float my)))
               (str (format "speed: %.6f" speed))
               (str (format "ar: %.2f" ar))
               (str (format "eps: %.8f" eps))
               (str (format "fov: %.2f"  fovdeg))
               (str (format "blend: %.2f" blend))
               (str (format "gamma: %.2f" gamma))
               (str (format "glow: %.2f" glow))
               ;(str "camera: " (state :camera))
               (str (format "fps: %.2f" (float (q/current-frame-rate))))
               ]]
    (q/fill 255 255 255 255)
    (doseq [i (range (count lines))]
      (q/text (lines i) x (+ y (* i line-space))))))


(defn draw-quad [state ar]
  (q/begin-shape :quads)
    (q/texture @tex1)
    (q/shader (get-in state [:current-shader :shaderobj]))
    (q/vertex -1 -1 0 0)
    (q/vertex  1 -1 ar 0)
    (q/vertex  1  1 ar 1)
    (q/vertex -1  1 0 1)
  (q/end-shape))



(defn draw [state]
  ;(q/ortho)
  ;(q/no-lights)
  (let [c [(* (q/width) 0.5)
           (* (q/height) 0.5)]]
    (q/with-translation c
      (q/scale (c 0) (c 1))
      ;(q/fill 0 0 0)
      (q/no-stroke)
      (draw-quad state (get state :aspect-ratio 1.0))

      )
    )
  (q/reset-shader) 
  (draw-info state 32 (- (q/height) 250))
  )


(defn -main [& args]
  (q/defsketch shaderpit
    :title "shader sandpit"
    :setup setup
    :draw draw
    ;:size [1900 1100]
    :size [1440 800]
    ;:size :fullscreen
    ;:features [:present
    ;           :resizable]
 
    :renderer :p2d
    ;:renderer :opengl
    :key-typed key-typed
    :update update
    :key-pressed key-pressed
    :key-released key-released
    :mouse-moved mouse-moved
    :mouse-dragged mouse-dragged
;    :mouse-pressed mouse-pressed
;    :mouse-released mouse-released
    :middleware [m/fun-mode]
    ))


;(-main)
