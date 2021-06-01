(ns shaderpit.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [jb.vector2 :as v2]
            [jb.vector3 :as v3]
            [shaderpit.metrics :as mtr]
            [shaderpit.console :as console]
            [shaderpit.util :as util]
            [shaderpit.transport :as t]
            [clojure.string :as str]
            [clojure.java.io :as io])
  (:import java.awt.event.KeyEvent
           (java.awt Robot)
           (java.awt MouseInfo)
           (java.lang System)
  ))

(def gr (atom nil))
(def ^:dynamic console-font)
(def ^:dynamic title-font)
(def tex1 (atom nil))
(def tex2 (atom nil))
(def ^:const PI Math/PI)
(def ^:const TWOPI (* PI 2.0))
(def ^:const SMALLPI (* PI 0.9))
(def ^:const SMALLPI2 (* SMALLPI 0.5))

(def robot (new Robot))
(def global-mouse (atom [0 0]))
(def global-pmouse (atom [0 0]))


; =========================================================================




(defn center-cursor []
  (.mouseMove robot (int (/ (q/screen-width) 2))
                    (int (/ (q/screen-height) 2))))


(defn make-save-state [state]
  (-> state
      (dissoc :aspect-ratio :keys-down :ns-delta :ns-prev
              :render-width :render-height :shaders)
      (update-in [:current-shader] dissoc :shaderobj)))


(defn save-shader-state [state shaderdef]
  (let [basename (shaderdef :name)
        statepath (str "./state/" basename ".state")
        savestate (make-save-state state)]
    (console/writeln (str "save state: " statepath))
    (spit statepath savestate)))



(defn next-shader [state]
  (console/writeln (str "NEXT"))
  (let [newidx  (mod (inc (state :current-shader-idx))
                     (count (state :shaders)))
        nextshader ((get state :shaders) newidx) ]
    (t/on-shader-change nextshader)
    (-> (util/init-with-shaderdef nextshader)
        (assoc :current-shader-idx newidx))))


(defn setup []
  (console/init)
  ;(q/smooth)
  (q/no-cursor)
  (q/texture-mode :normal)
  (q/texture-wrap :repeat)
  (q/noise-detail 2)
  (q/hint :disable-depth-test)
  (q/frame-rate 60)
  (reset! gr (q/create-graphics (int (/ (q/width) 2))
                                (int (/ (q/height) 2))
                                :p2d))

  (util/glsl-watcher-init)

  (mtr/init)
  (mtr/init-graphics)
  (t/init)
  ;(def console-font (q/load-font "data/FreeMono-16.vlw"))
  (def console-font (q/load-font "data/app/fonts/AmericanTypewriter-24.vlw"))
  (def title-font (q/load-font "data/app/fonts/Courier-Bold-64.vlw"))
  ;(reset! tex1 (q/load-image "testpattern4po6.png"))
  ;(reset! tex1 (q/load-image "UV_Grid_Sm.jpg"))
  ;(reset! tex1 (q/load-image "uv_checker_large.png"))
  ;(reset! tex1 (q/load-image "Sky02-blur128x12.jpg"))
  ;(reset! tex1 (q/load-image "beach-hdr-blur128.jpg"))
  (reset! tex1 (q/load-image "sphericalsky-b.jpg"))
  ;(reset! tex1 (q/load-image "cube-grid.png"))
  ;(reset! tex1 (q/load-image "cubesphere.jpg"))
  ;(reset! tex1 (q/load-image "stereographic.jpg"))
  ;(reset! tex1 (q/load-image "sphere_map_floor_gradient.jpg"))
  (let [shaderlist (util/load-shader-dir)]
    (util/init-with-shaderdef (first shaderlist))
  ))


(defn update-uniforms-common! [state shader]
  (let [mp (get state :mouse-position [0 0])
        ar (state :aspect-ratio)]
    (.set shader "framecount" (float (q/frame-count)))
    (.set shader "aspect_ratio" (float ar))
    (.set shader "resolution" (float (state :render-width))
                              (float (state :render-height)))
    (.set shader "time" (float (util/t-now state)))
    (.set shader "dt" (float (util/t-delta state)))))


(defn update-uniforms-3d! [state shader]
  (let [mp (get state :mouse-position [0 0])
        ;ar (state :aspect-ratio)
        cam-pos (get-in state [:camera :pos])
        cam-lookat (get-in state [:camera :lookat])
        cam-fov (get-in state [:camera :fov])
        ]
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
    (.set shader "swidth" (float (state :render-width)))
    (.set shader "sheight" (float (state :render-height)))
    (.set shader "tex1" @tex1)
  ))


(defn update-uniforms-2d! [state shader]
  (let [mp (get state :mouse-position [0 0])]
    (.set shader "mouse" (float (mp 0)) (float (mp 1)))
    (.set shader "zoom" (float (get-in state [:camera :zoom] 1.0)))
    (.set shader "tex1" @gr)
    ; TODO viewport offset
    ; TODO viewport rotation
    ))


(defn update-uniforms! [state]
  (when state
    (let [shader (get-in state [:current-shader :shaderobj])]
      (update-uniforms-common! state shader)
      (if (= (state :camera-model) :3d)
        (update-uniforms-3d! state shader)
        (update-uniforms-2d! state shader))))
  state)


(defn get-global-mouse []
  (let [pi (MouseInfo/getPointerInfo)
        loc (.getLocation pi)
        pos [(.x loc) (.y loc)]]
    pos))


(defn wrap-n [n, nmin nmax]
  (let [r (- nmax nmin)]
    (if (>= n nmax)
      (- n r)
      (if (< n nmin)
        (+ n r)
        n))))


(defn camera-drift [cam t dt]
  (let [wup [0.0 -1.0 0.0] ; world up
        vpn (cam :vpn)
        vpr (v3/normalize (v3/cross vpn wup))
        s (* t 0.1)
        mag 0.1
        n1 (* (- (q/noise s) 0.5) 2.0)
        n2 (* (- (q/noise (+ s 12.34)) 0.5) 2.0)
        n (v3/add (v3/scale wup n1)
                  (v3/scale vpr n2))
        ;n (v3/scale n mag)
        acc (v3/scale n dt) ]
    ;(when (= (mod (q/frame-count) 15) 0)
    ;  (console/writeln (format "n: [%.3f  %.3f  %.3f]" (n 0) (n 1) (n 2))))
    (update-in cam [:vel] v3/add acc)))


(defn camera-update [state]
  (let [t (util/t-now state)
        dt (util/t-delta state)
        cam (state :camera)
        drift_WIP (camera-drift cam t dt)
        [mx my] (v2/scale (v2/sub (get-global-mouse)
                                  [(int (/ (q/screen-width) 2))
                                   (int (/ (q/screen-height) 2))])
                           0.5) ; mouse sensitivity factor
        mx (if (and (state :mousewarp) (q/focused)) mx 0.0)
        my (if (and (state :mousewarp) (q/focused)) my 0.0)
        az-vel  (+ (cam :az-vel) (* mx dt))
        alt-vel (+ (cam :alt-vel) (* my dt))
        dampr (cam :damp-r)
        dampr-dt (Math/pow dampr dt)
        dampm (cam :damp-m)
        dampm-dt (Math/pow dampm dt)

        az (+ (cam :az)
              (/ (* az-vel (- dampr-dt 1.0))
                 (Math/log dampr)))
        az-vel (* az-vel dampr-dt)

        alt (+ (cam :alt)
               (/ (* alt-vel (- dampr-dt 1.0))
                  (Math/log dampr)))
        alt-vel (* alt-vel dampr-dt)
        az (mod az TWOPI)
        ;az (wrap-n az 0.0 TWOPI)
        alt (q/constrain alt (- SMALLPI2) SMALLPI2)

        vpn (v3/normalize [(* (Math/cos alt) (Math/cos az))
                           (Math/sin alt)
                           (* (Math/cos alt) (Math/sin az))])

        pos (v3/add (cam :pos)
                    (v3/scale (v3/scale (cam :vel) (- dampm-dt 1.0))
                              (/ 1.0 (Math/log dampm))))

        vel (v3/scale (cam :vel) dampm-dt)

        lookat (v3/add pos (v3/scale vpn 6.0))

        new-cam (-> (state :camera)
                    (assoc :az az)
                    (assoc :alt alt)
                    (assoc :az-vel az-vel)
                    (assoc :alt-vel alt-vel)
                    (assoc :vpn vpn)
                    (assoc :lookat lookat)
                    (assoc :pos pos)
                    (assoc :vel vel)
                    ) ]

  (when (and (state :mousewarp) (q/focused))
    (center-cursor))

  (-> state
      (assoc :camera new-cam))))


(defn camera-reset [state]
  (-> state
      (assoc :camera util/initial-camera)
      (assoc :aspect-ratio (/ (float (q/width)) (q/height))))
  )


(defn render-start! [state]
  (when (= (state :camera-model) :3d)
    (center-cursor))
  (q/start-loop)
  (when (= (state :camera-model) :3d)
    (q/no-cursor))
    (-> state
      (assoc :render-paused? false)
      (assoc :ns-prev (util/ns-time))
      (assoc-in [:mousewarp] (= (state :camera-model) :3d))))


(defn render-pause! [state]
  (q/no-loop)
  (q/cursor)
  (-> state
     (assoc :render-paused? true)
     (assoc-in [:mousewarp] false)))


(defn do-key-movement [state keychar]
  (let [cam (state :camera)
        pos-old  (cam :pos)
        wup [0.0 -1.0 0.0] ; world up
        vpn (cam :vpn)
        vpv (v3/normalize (v3/cross (cam :vpn) [0.0 -1.0 0.0]))
        vel (cam :vel)
        acc (* (cam :speed) (util/t-delta state))
        nacc (- acc)
        key-movement-map {
          \w (fn [s] (assoc-in s [:camera :vel] (v3/add vel (v3/scale vpn acc))))
          \s (fn [s] (assoc-in s [:camera :vel] (v3/add vel (v3/scale vpn nacc))))
          \a (fn [s] (assoc-in s [:camera :vel] (v3/add vel (v3/scale vpv acc))))
          \d (fn [s] (assoc-in s [:camera :vel] (v3/add vel (v3/scale vpv nacc))))
          \e (fn [s] (assoc-in s [:camera :vel] (v3/add vel (v3/scale wup nacc))))
          \c (fn [s] (assoc-in s [:camera :vel] (v3/add vel (v3/scale wup acc))))
          \x (fn [s] (assoc-in s [:camera :vel] (v3/scale vel 0.9)))
          \1 (fn [s] (update-in s [:camera :speed] #(* % 0.99)))
          \2 (fn [s] (update-in s [:camera :speed] #(/ % 0.99)))
          \- (fn [s] (update-in s [:camera :fov] #(* % 0.99)))
          \= (fn [s] (update-in s [:camera :fov] #(/ % 0.99)))
          \f (fn [s] (update-in s [:camera :damp-r] #(* % 0.99)))
          \t (fn [s] (update-in s [:camera :damp-r] #(/ % 0.99)))
          \g (fn [s] (update-in s [:camera :damp-m] #(* % 0.99)))
          \y (fn [s] (update-in s [:camera :damp-m] #(/ % 0.99)))
          \b (fn [s] (update-in s [:params :blend_coef] #(- % 0.05)))
          \n (fn [s] (update-in s [:params :blend_coef] #(+ % 0.05)))
          \[ (fn [s] (update-in s [:params :ray_hit_epsilon] #(* % 0.9)))
          \] (fn [s] (update-in s [:params :ray_hit_epsilon] #(min 0.01 (/ % 0.9))))
          \3 (fn [s] (update-in s [:params :palette_offset] #(- % 0.005)))
          \4 (fn [s] (update-in s [:params :palette_offset] #(+ % 0.005)))
          \5 (fn [s] (update-in s [:params :gamma] #(- % 0.01)))
          \6 (fn [s] (update-in s [:params :gamma] #(+ % 0.01)))
          \7 (fn [s] (update-in s [:params :glow-intensity] #(- % 0.01)))
          \8 (fn [s] (update-in s [:params :glow-intensity] #(+ % 0.01)))
          ;\9 (fn [s] update-in s [:pixel-scale] #(int (max (- % 1) 1)))
          ;\0 (fn [s] update-in s [:pixel-scale] #(int (+ % 1)))
          \l (fn [s] (update-in s [:params :diff-spec] #(max 0.0 (- % 0.01))))
          \p (fn [s] (update-in s [:params :diff-spec] #(min 1.0 (+ % 0.01))))
          ;KeyEvent/VK_ALT (fn [s] (do 
          ;                (println "ALT")
          ;                s))
         }]
    (if (contains? key-movement-map keychar)
      ((key-movement-map keychar) state)
      state)))


(defn do-movement-keys [state & keys-down]
  (if (nil? keys-down)
    (recur state (state :keys-down))
    (if (<= (count keys-down) 0)
      state
      (recur (do-key-movement state (first keys-down))
             (rest keys-down)))))


(defn key-pressed [state event]
  ;(console/writeln (str "key: " event))
  (when (= 27 (q/key-code))
    ; dont exit
    (set! (.key (quil.applet/current-applet)) (char 0)))
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
           (render-start! state)
           (render-pause! state)))
    \# (do
         (q/save-frame)
         state)
    \R (do
         (camera-reset state))
    \0 (do (-> state
               (assoc :aspect-ratio (/ (float (q/width)) (q/height)))
               (assoc-in [:camera :pos] [0.0 0.0 0.0])))
    \m (do
         (if (state :mousewarp) ; turning mousewarp OFF
           (q/cursor)
           (do                  ; turning mousewarp ON
             (center-cursor)
             (q/no-cursor)))
         (-> state
             (update-in [:mousewarp] not)))
    \M (do
         (-> state
             (assoc :camera-model
                    (if (= (state :camera-model) :2d) :3d :2d) )
             (assoc :mousewarp (= (state :camera-model) :3d))))
    \` (do 
            (-> state
                (util/clock-reset)
                (assoc-in [:current-shader :shaderobj]
                          (q/load-shader (get-in state [:current-shader :path])))))
    \/ (do
         (if (t/recording?)
           state
           (next-shader state)))
    \! (do
        (save-shader-state state (state :current-shader))
        state)
    \P (do
         (t/on-record state)
         state)
    \p (do
         (t/on-play-pause)
         (if (t/playing?)
           (update state :mousewarp :false)
           (update state :mousewarp :true)))
    state))


(defn mouse-moved [state event]
  (-> state
      ;(assoc :mouse-position [(int (/ (event :x) (q/width)))
      ;                        (int (/ (event :y) (q/height)))])
      ))


(defn mouse-dragged [state event]
  (let [p [(/ (float (event :x)) (q/width))
           (/ (float (event :y)) (q/height))]
        p [(p 0) (- 1.0 (p 1))]]
    ;(console/writeln (format "mouse: %s" (v2/format p)))
    (-> state
        (assoc :mouse-position p))))


(defn mouse-wheel [state r]
  (console/writeln (format "mousewheel: %s" r))
  (if (and
        (get-in state [:camera :zoom])
        (= (state :camera-model) :2d))
    (update-in state [:camera :zoom] #(+ % (* r 0.1)))
    state))


(defn handle-resize [state]
  (if (or (not= (state :render-width) (int (/ (q/width) 2)))
          (not= (state :render-height) (int (/ (q/height) 2))))
    (do
      (.dispose @gr)
      (reset! gr (q/create-graphics (int (/ (q/width) 2))
                                    (int (/ (q/height) 2))  :p2d))
      (console/writeln (format "resize %sx%s" (.width @gr) (.height @gr)))
      (-> state
          (assoc :render-width (int (/ (q/width) 2)))
          (assoc :render-height (int (/ (q/height) 2)))
          (assoc :aspect-ratio (/ (float (q/width)) (q/height)))))
    state))


(defn state-update [state]
  (console/update!)
  (if (t/playing?)
    (-> state
        (handle-resize)
        (t/current-frame)
        (update-uniforms!))
    (-> state
        (util/debounce-watcher-events)
        (handle-resize)
        (util/clock-tick)
        (do-movement-keys)
        (camera-update)
        (update-uniforms!)
        (t/capture-frame))))


(defn draw-info [state x y]
  (q/text-font console-font)
  (let [line-space 30
        ar (get state :aspect-ratio 0.0)
        [mx my] (state :mouse-position)
        zoom (get-in state [:camera :zoom] 0.0)
        pos (get-in state [:camera :pos] [0.0 0.0 0.0])
        az (get-in state [:camera :az] 0.0)
        alt (get-in state [:camera :alt] 0.0)
        vpn (get-in state [:camera :vpn] [0.0 0.0 0.0])
        dampr (get-in state [:camera :damp-r] 0.0)
        dampm (get-in state [:camera :damp-m] 0.0)
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
        title-height 50
        lines [
               ;(str "state: " state)
               ;(str "shader: " shadername)
               (str (format "dim: %dx%d" (state :render-width) (state :render-height)))
               (str (format "fov: %.2f"  fovdeg))
               (str (format "ar: %.2f" ar))
               (str (format "zoom: %.2f" zoom))

               (str (format "camera: %s" (state :camera-model)))
               (str "pos: " (v3/format pos))
               (str (format "speed: %.6f" speed))
               (str "vpn: " (v3/format vpn))
               (str (format "az: %.2f" az))
               (str (format "alt: %.2f" alt))
               (str (format "dampm: %.4f" dampm))
               (str (format "dampr: %.4f" dampr))
               (str (format "mouse: [%.2f %.2f]" (float mx) (float my)))
               (str (format "eps: %.8f" eps))
               (str (format "blend: %.2f" blend))
               (str (format "gamma: %.2f" gamma))
               (str (format "glow: %.2f" glow))
               (str (format "pal: %.2f" pal-off))
               (str (format "d/s: %.2f" diff_spec))
               ;(str "camera: " (state :camera))
               (str (format "time %.2f" (util/t-now state)))
               (str (format "dt %.5f" (util/t-delta state)))
               (str (format "fps: %.2f" (float (q/current-frame-rate))))
               ]]
    ;(q/fill 255 255 255 255)
    (q/no-stroke)
    (q/fill 255 255 255 192)
    (q/text-font title-font)
    (q/text shadername x y)
    (q/text-font console-font)
    (doseq [i (range (count lines))]
      (q/fill 0 0 0 128)
      (q/rect (- x 6) (- (+ title-height y (* i line-space)) 20)
              (+ (q/text-width (lines i)) 12) 26 12)
      (q/fill 255 255 255 192)
      (q/text (lines i) x (+ title-height y (* i line-space))))))
    ;(doseq [i (range (count lines))]
    ;  (q/text (lines i) x (+ y (* i line-space))))))


(defn draw-quad [state ar]
  (q/begin-shape :quads)
    (q/vertex -1 -1 0 0)
    (q/vertex  1 -1 ar 0)
    (q/vertex  1  1 ar 1)
    (q/vertex -1  1 0 1)
  (q/end-shape))


(defn draw-quad-uv01 [state ar]
  (q/begin-shape :quads)
    (q/vertex -1 -1  0.0  1.0)
    (q/vertex  1 -1  1.0  1.0)
    (q/vertex  1  1  1.0  0.0)
    (q/vertex -1  1  0.0  0.0)
  (q/end-shape))


(defn draw [state]
  (let [rc [(* (state :render-width) 0.5) (* (state :render-height) 0.5)]
        sc [(* (q/width) 0.5) (* (q/height) 0.5)]
        shd (get-in state [:current-shader :shaderobj])
        ar (get state :aspect-ratio 1.0)
        t-render-start (System/nanoTime)
        t (util/t-now state)
        ]
    (q/with-graphics @gr
      (q/texture-wrap :repeat)
      (q/with-translation rc
        (q/scale (sc 0) (sc 1))
        ;(when (q/loaded? shd)
          (q/shader shd)
          ;)
        (if (= (state :camera-model) :3d)
          (draw-quad state ar)
          (draw-quad-uv01 state ar))))
    
    (mtr/capture :fps (q/current-frame-rate))
    (mtr/capture :t-frame (util/t-delta state))
    (q/reset-shader)
    (q/image @gr 0 0 (q/width) (q/height))
    (mtr/capture :t-render (double (/ (- (System/nanoTime) t-render-start) 1000000000)))
    
    (mtr/draw-all (- (q/width) mtr/width 20) 20)
    (t/draw-ui (/ (q/width) 2) 40, t)
    (draw-info state 20 50)
    (q/fill 255)
    (q/no-tint)
    (q/image (console/get-image) 0 (- (q/height) (console/size 1) 10))
  ))


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
    :settings #(q/smooth 8)
    ;:settings #(q/pixel-density 2)
    ;:settings #(q/pixel-density (q/display-density)) 
    :renderer :p2d
    ;:renderer :opengl
    :key-typed key-typed
    :update state-update
    :key-pressed key-pressed
    :key-released key-released
    :mouse-moved mouse-moved
    :mouse-dragged mouse-dragged
    :mouse-wheel mouse-wheel
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
