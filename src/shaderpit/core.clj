(ns shaderpit.core
  (:require [quil.core :as q]
            [quil.middleware :as m])
  (:use [shaderpit.vector])
  (:import java.awt.event.KeyEvent
           (java.awt Robot)
           (java.awt MouseInfo)
           (java.lang System)
  ))


(def ^:dynamic console-font)
(def tex1 (atom nil))
(def tex2 (atom nil))
(def ^:const PI Math/PI)
(def ^:const TWOPI (* PI 2.0))
(def ^:const SMALLPI (* PI 0.99))
(def ^:const SMALLPI2 (* SMALLPI 0.5))

(def robot (new Robot))
(def global-mouse (atom [0 0]))
(def global-pmouse (atom [0 0]))

; =========================================================================

(def default-shader {
  :id 0
  :name "Unnamed"
  :path ""
  :type :fragment
  :shaderobj nil
  })

(defn ns-time [state]
  (- (double (/ (System/nanoTime) 1000000000.0)) (state :t-init)))


(defn clock-reset [state]
  (-> state
      (assoc :t-init 0.0)
      (assoc :t-now (ns-time state))
      (assoc :t-delta 0.0)))


(defn clock-tick [state]
  (let [t-new (ns-time state)
        dt (- t-new (state :t-now))]
    (-> state
      (assoc :t-now t-new)
      (assoc :t-delta dt))))


(defn center-cursor []
  (.mouseMove robot (int (/ (q/screen-width) 2))
                    (int (/ (q/screen-height) 2))))

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
  :az 0.0
  :alt 0.0
  :az-vel 0.0
  :alt-vel 0.0
  :lookat [0.0 0.0 0.0]
  :vpn [0.0 0.0 -1.0]
  :fov (/ Math/PI 3.0)
  :vel 0.0
  :speed 1.0
  :damping 0.0005
})

(def initial-params {
  :blend_coef 10.0
  :ray_hit_epsilon 0.01
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
  :aspect-ratio 1.0
  :render-paused? false
  :camera initial-camera
  :params initial-params
  :shaders {}
  :current-shader nil
  :t-now 0.0
  :t-init 0.0
  :t-delta 0.0
})




(defn setup []
  (let [shaderlist (load-shader-dir)
        current-shader (first shaderlist)
        shaderobj (q/load-shader (current-shader :path))  ]
    ;(q/smooth)
    (q/no-cursor)
    (q/texture-mode :normal)
    (q/texture-wrap :repeat)
    (q/noise-detail 2)
    (q/hint :disable-depth-test)
    (q/frame-rate 60)
    (def console-font (q/load-font "data/FreeMono-16.vlw"))
    ;(reset! tex1 (q/load-image "testpattern4po6.png"))
    ;(reset! tex1 (q/load-image "UV_Grid_Sm.jpg"))
    ;(reset! tex1 (q/load-image "uv_checker_large.png"))
    ;(reset! tex1 (q/load-image "Sky02.jpg"))
    ;(reset! tex1 (q/load-image "Sky02-blur128x12.jpg"))
    ;(reset! tex1 (q/load-image "North_South_Panorama_Equirect_360x180.jpg"))
    ;(reset! tex1 (q/load-image "QueensPark.m.jpg"))
    ;(reset! tex1 (q/load-image "beach-hdr-blur128.jpg"))
    ;(reset! tex1 (q/load-image "cubesphere.jpg"))
    ;(reset! tex1 (q/load-image "stereographic.jpg"))
    (reset! tex1 (q/load-image "sphere_map_floor_gradient.jpg"))
    ;(reset! tex1 (q/load-image "cave_texture_01-512x512.png"))
    ;(reset! tex1 (q/load-image "seamless-black-wall-texture-decorating-inspiration-1.jpg"))
    ;(reset! tex1 (q/load-image "beach-hdr.jpg"))
    ;(reset! tex1 (q/load-image "studio010.jpg"))
    
    ;(reset! texture-shader (load-shader "data/texfrag.glsl" "data/texvert.glsl"))
    (-> initial-state
         (assoc :aspect-ratio (/ (float (q/width)) (q/height)))
         (assoc :shaders shaderlist)
         (assoc :current-shader (assoc current-shader :shaderobj shaderobj))
         (clock-reset)
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
      (.set shader "diff_spec" (float (get-in state [:params :diff-spec] 0.5)))
      (.set shader "time" (float (state :t-now)))
      ;(.set shader "time" (float (/ (q/millis) 1000.0)))

      )
    state))


(defn get-global-mouse []
  (let [pi (MouseInfo/getPointerInfo)
        loc (.getLocation pi)
        pos [(.x loc) (.y loc)]]
    pos))



(defn update-cam-rotation [cam dt]
  (let [az-vel (cam :az-vel)
        alt-vel (cam :alt-vel)
        az-dist (- az-vel (* az-vel (cam :damping)))
        az-dist-delta (* az-dist dt)
        az-new (+ az-vel az-dist-delta)
        
        alt-dist (- alt-vel (* alt-vel (cam :damping)))
        alt-dist-delta (* alt-dist dt)
        alt-new (+ alt-vel alt-dist-delta)
        ]

      (-> cam
          (assoc :az az-new)
          (assoc :alt alt-new))
    )
)


; 
; D = damping
; dx = mouse delta
; 
; V' = (V + (dx * dt))
; Vdammped = V' * (1 - (V' - (V' * D)) * dt)
; 
; 
; 
; 
;(defn camera-update2 [state]
;  (let [cam (state :camera)
;        dt (state :t-delta)
;        [mx my] (vec2-scale (vec2-sub (get-global-mouse)
;                                      [(int (/ (q/screen-width) 2))
;                                       (int (/ (q/screen-height) 2))])
;                            0.005)
;
;        az-vel  (+ (cam :az-vel) (* mx dt))
;        vx (* (- az-vel (* az-vel (cam :damping))) dt)
;        az-vel (* az-vel (- 1.0 vx))
;        
;        alt-vel  (+ (cam :alt-vel) (* my dt))
;        vx (* (- alt-vel (* alt-vel (cam :damping))) dt)
;        alt-vel (* alt-vel (- 1.0 vx))
;
;        az (+ (cam :az) az-vel)
;        alt (+ (cam :alt) alt-vel)
;        ;az (+ (cam :az) mx)
;        ;alt (+ (cam :alt) my)
;        az (if (>= az TWOPI)
;             (- az TWOPI)
;             (if (< az 0.0)
;              (+ az TWOPI)
;              az))
;        spi2 (* SMALLPI 0.5)
;        alt (if (>= alt spi2)
;             spi2
;             (if (<= alt (- spi2))
;              (- spi2)
;              alt))
;        vpn (vec3-normalize [(* (Math/cos alt) (Math/cos az))
;                             (Math/sin alt)
;                             (* (Math/cos alt) (Math/sin az))])
;        lookat (vec3-add (cam :pos) (vec3-scale vpn 6.0))
;        new-cam (-> (state :camera)
;                    (assoc :az az)
;                    (assoc :alt alt)
;                    (assoc :az-vel az-vel)
;                    (assoc :alt-vel alt-vel)
;                    (assoc :vpn vpn)
;                    (assoc :lookat lookat)
;                    ) ]
;
;  (when (and (state :mousewarp) (q/focused))
;    (center-cursor))
;
;  (-> state
;      (assoc :camera new-cam))))

(defn wrap-n [n, nmin nmax]
  (let [r (- nmax nmin)]
    (if (>= n nmax)
      (- n r)
      (if (< n nmin)
        (+ n r)
        n))))


(defn camera-update [state]
  (let [cam (state :camera)
        [mx my] (vec2-scale (vec2-sub (get-global-mouse)
                                      [(int (/ (q/screen-width) 2))
                                       (int (/ (q/screen-height) 2))])
                            0.5) ; mouse sensitivity factor
        dt (state :t-delta)
        az-vel  (+ (cam :az-vel) (* mx dt))
        alt-vel (+ (cam :alt-vel) (* my dt))
        damp (cam :damping)

        az (+ (cam :az) (/ (* az-vel
                       (- (Math/pow damp dt) 1.0))
                    (Math/log damp)))
        az-vel (* az-vel (Math/pow damp dt))

        alt (+ (cam :alt) (/ (* alt-vel
                         (- (Math/pow damp dt) 1.0))
                      (Math/log damp)))
        alt-vel (* alt-vel (Math/pow damp dt))

        az (wrap-n az 0.0 TWOPI)
        alt (q/constrain alt (- SMALLPI2) SMALLPI2)

        vpn (vec3-normalize [(* (Math/cos alt) (Math/cos az))
                             (Math/sin alt)
                             (* (Math/cos alt) (Math/sin az))])
        lookat (vec3-add (cam :pos) (vec3-scale vpn 2.0))
        new-cam (-> (state :camera)
                    (assoc :az az)
                    (assoc :alt alt)
                    (assoc :az-vel az-vel)
                    (assoc :alt-vel alt-vel)
                    (assoc :vpn vpn)
                    (assoc :lookat lookat)
                    ) ]

  (when (and (state :mousewarp) (q/focused))
    (center-cursor))

  (-> state
      (assoc :camera new-cam))))


(defn do-key-movement [state keychar]
  (let [cam (state :camera)
        pos-old  (cam :pos)
        wup [0.0 -1.0 0.0] ; world up
        vpn (cam :vpn)
        vpv (vec3-normalize (vec3-cross (cam :vpn) [0.0 -1.0 0.0]))
        speed (* (cam :speed) (state :t-delta))
        ;speed (get-in state [:camera :speed])
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
          \- (fn [s] (update-in s [:camera :fov] #(* % 0.99)))
          \= (fn [s] (update-in s [:camera :fov] #(/ % 0.99)))
          \f (fn [s] (update-in s [:camera :damping] #(* % 0.99)))
          \t (fn [s] (update-in s [:camera :damping] #(/ % 0.99)))
          \[ (fn [s] (update-in s [:params :ray_hit_epsilon] #(* % 0.9)))
          \] (fn [s] (update-in s [:params :ray_hit_epsilon] #(min 0.01 (/ % 0.9))))
          \3 (fn [s] (update-in s [:params :palette_offset] #(- % 0.005)))
          \4 (fn [s] (update-in s [:params :palette_offset] #(+ % 0.005)))
          \5 (fn [s] (update-in s [:params :gamma] #(- % 0.01)))
          \6 (fn [s] (update-in s [:params :gamma] #(+ % 0.01)))
          \7 (fn [s] (update-in s [:params :glow-intensity] #(- % 0.01)))
          \8 (fn [s] (update-in s [:params :glow-intensity] #(+ % 0.01)))
          \l (fn [s] (update-in s [:params :diff-spec] #(max 0.0 (- % 0.01))))
          \p (fn [s] (update-in s [:params :diff-spec] #(min 1.0 (+ % 0.01))))
         }]
    ;state))
  (if (contains? key-movement-map keychar)
    (-> ((key-movement-map keychar) state)
        ;;(camera-update)
        )
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
      

(defn key-released [state e]
  (-> state
      (assoc :keys-down (disj (state :keys-down) (q/raw-key)))))


(defn key-typed [state event]
  (case (event :raw-key)
    \  (do
         (if (state :render-paused?)
           (do 
             (q/start-loop)
             (q/no-cursor))
           (do
             (q/no-loop)
             (q/cursor)))
         (center-cursor)
         (-> state
          (update-in [:render-paused?] not)
          (assoc-in [:mousewarp] (state :render-paused?))))
    \# (do
         (q/save-frame)
         state)
    \R (do (-> state
               (assoc :camera initial-camera)
               (assoc :aspect-ratio (/ (float (q/width)) (q/height)))))
;    \R (do (-> initial-state
;               (assoc :aspect-ratio (/ (float (q/width)) (q/height)))))
    \0 (do (-> state
               (assoc :aspect-ratio (/ (float (q/width)) (q/height)))
               (assoc-in [:camera :pos] [0.0 0.0 0.0])))
    \m (do
         (if (state :mousewarp)
           (q/cursor)
           (q/no-cursor)) 
         (-> state
             (update-in [:mousewarp] not)))
    \` (do 
            (-> state
                (clock-reset)
                (assoc-in [:current-shader :shaderobj]
                          (q/load-shader (get-in state [:current-shader :path])))))
    \/ (do
            (-> state
                (clock-reset)
                (next-shader)))
    state))



(defn mouse-moved [state event]
  (-> state
      (assoc :mouse-position [(int (/ (event :x) (q/width)))
                              (int (/ (event :y) (q/height)))])))
       

(defn mouse-dragged [state event]
    (-> state
        (assoc :mouse-position [(event :x) (event :y)])))



(defn update [state]
  (-> state
      (clock-tick)
      (do-movement-keys)
      (camera-update)
      (update-uniforms! (get-in state [:current-shader :shaderobj]))
  ))


(defn draw-info [state x y]
  (q/text-font console-font)
  (let [line-space 20
        ar (get state :aspect-ratio 0.0)
        [mx my] (state :mouse-position)
        zoom (get state :zoom 0.0)
        pos (get-in state [:camera :pos] [0.0 0.0 0.0])
        az (get-in state [:camera :az] 0.0)
        alt (get-in state [:camera :alt] 0.0)
        damp (get-in state [:camera :damping] 0.0)
        speed (get-in state [:camera :speed] 0.0)
        fov (get-in state [:camera :fov] 0.0)
        fovdeg (/ (* fov 180.0) Math/PI)
        blend (get-in state [:params :blend_coef] 0.0)
        eps (get-in state [:params :ray_hit_epsilon] 0.0)
        gamma (get-in state [:params :gamma] 0.0)
        glow (get-in state [:params :glow-intensity] 0.0)
        shadername (get-in state [:current-shader :name] "-")
        pal-off (get-in state [:params :palette_offset] 0.0)
        diff_spec (get-in state [:params :diff-spec] 0.5)
        lines [
               ;(str "state: " state)
               (str (format "time %.2f" (state :t-now)))
               (str (format "dt %.5f" (state :t-delta)))
               (str (format "dim: %dx%d" (q/width) (q/height)))
               (str "shader: " shadername)
               (str "pos: " (vec3-format pos))
               (str (format "az: %.2f" az))
               (str (format "alt: %.2f" alt))
               (str (format "damp: %.4f" damp))
               (str (format "mouse: [%.2f %.2f]" (float mx) (float my)))
               (str (format "speed: %.6f" speed))
               (str (format "ar: %.2f" ar))
               (str (format "eps: %.8f" eps))
               (str (format "fov: %.2f"  fovdeg))
               (str (format "blend: %.2f" blend))
               (str (format "gamma: %.2f" gamma))
               (str (format "glow: %.2f" glow))
               (str (format "pal: %.2f" pal-off))
               (str (format "d/s: %.2f" diff_spec))
               ;(str "camera: " (state :camera))
               (str (format "fps: %.2f" (float (q/current-frame-rate))))
               ]]
    (q/fill 255 255 255 255)
    (doseq [i (range (count lines))]
      (q/fill 0 0 0 128)
      (q/rect (- x 6) (- (+ y (* i line-space)) 12)
              (+ (q/text-width (lines i)) 12) 16 9)
      (q/fill 255 255 255 255)
      (q/text (lines i) x (+ y (* i line-space))))))
    ;(doseq [i (range (count lines))]
    ;  (q/text (lines i) x (+ y (* i line-space))))))


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
  (draw-info state 32 (- (q/height) 370))
  )


(defn -main [& args]
  (q/defsketch shaderpit
    :title "shader sandpit"
    :setup setup
    :draw draw
    :size [1920 1080]
    ;:size [1440 800]
    ;:size :fullscreen
    :features [:resizable]
    ;:features [:present :resizable]
    ;:settings #(q/pixel-density 2)
    ;:settings #(q/pixel-density (q/display-density)) 
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


(defn get-state [& prop]
  ;@((meta shaderpit) :state)
  (if prop
    (get-in @((meta shaderpit) :state) prop)
     @((meta shaderpit) :state))

  )


;(-main)
