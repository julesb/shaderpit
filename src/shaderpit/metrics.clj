(ns shaderpit.metrics
  (:require [quil.core :as q]))


(def metrics (atom nil))
(def ^:const bufsize 256)
(def ^:const width 256)
(def ^:const height 100)
(def ^:dynamic title-font)

(def metricdefs {
  :t-render {
    :label "Render"
    :values [0 0 0]
    :minval 0.0
    :maxval (/ 1.0 60.0)
    :g nil
  }
  :t-frame {
    :label "Frame"
    :values [0 0 0]
    :minval 0.0
    :maxval (/ 1.0 30.0)
    :g nil
  }
  :fps {
    :label "FPS"
    :values [0 0 0]
    :minval 0.0
    :maxval 120.0
    :g nil
  }
})


(defn init-graphics []
  (def title-font (q/load-font "data/FreeMono-16.vlw"))
  (doseq [k (keys @metrics)]
    (swap! metrics assoc-in [k :g] (q/create-graphics width height :p2d))
  
  ))

(defn init []
  (reset! metrics metricdefs) )


(defn capture [metrickey value]
  (if (>= (count (get-in @metrics [metrickey :values])) bufsize)
    (swap! metrics #(-> %
                        (update-in [metrickey :values] (fn [vs] (vec (rest vs))))
                        (update-in [metrickey :values] conj value)))
    (swap! metrics update-in [metrickey :values] conj value)))


(defn draw [mkey x y]
  (let [m (@metrics mkey)
        rng (- (m :maxval) (m :minval))
        v (last (m :values))
        s (/ (float height) rng)
        ys (* v s)
        g (m :g)]
    
    (q/image-mode :corner)
    (q/with-graphics g
      (q/copy [1 0 (- width 1) height]
              [0 0 (- width 1) height])
      (q/stroke-weight 1)
      (q/stroke 0 0 0)
      (q/line (- width 1) 0 (- width 1) (- height 1))
      (q/stroke 255 255 255)
      (q/point (- width 1) (- height 1 ys) )
      ;(q/line (- width 1) (- height 1) (- width 1) (- height 1 ys))
      )
    (q/image g x y)
  ))


(defn draw-all [x y]
  (let [mks (vec (keys @metrics))
        margin 10]
    (q/text-font title-font)
    (doseq [i (range (count mks))]
      (q/tint 255 255 255 192)
      (draw (mks i) x (+ y (* i height) (* i margin)))
      (q/no-fill)
      (q/stroke 128 128 128 64)
      (q/rect x (+ y (* i height) (* i margin)) width height)
      (q/fill 255)
      (q/text ((@metrics (mks i)) :label)
            (+ x 1) (+ y 11 (* i height) (* i margin)))
    )
  ))


