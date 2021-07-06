(ns shaderpit.spectrogram
  (:require [quil.core :as q]))


(def gr (atom nil))
(def width (atom 256))
(def height (atom 256))


(defn init [w h]
  (reset! width w)
  (reset! height h)
  (reset! gr (q/create-graphics w h :p2d))
  (q/with-graphics @gr
    (q/background 0 0 0 0)))


(defn draw [x y spectrum]
  (q/image-mode :corner)
  (q/with-graphics @gr
    (q/blend-mode :replace)
    ;(q/color-mode :rgb 255 255 255 255)
    (q/copy [1 0 (- @width 1) @height]
            [0 0 (- @width 1) @height])
    (q/stroke-weight 1)
    (q/stroke 0 0 0)
    (doseq [i (range @height)]
      (let [s (int (* (spectrum i) 255.0))
            s (* s 5.0) ; fudge
            ]
        (q/set-pixel (- @width 1)
                     (- @height 1 i)
                     (q/color 255 255 255 s)
                     ;(q/color s s s)
                     ))))
  (q/image @gr x y (* @width 4) (* @height 2)))


(defn draw-fft [x y w h spectrum]
  (let [downscale 2
        bands (/ (count spectrum) downscale)]
    (q/no-fill)
    (q/stroke-weight 2)
    (q/stroke 255)
    (doseq [i (range bands)]
      (let [sy (spectrum i) ]
        (q/line (+ x (* i downscale))
                (+ y h)
                (+ x (* i downscale))
                (- (+ y h) (* sy h)))))))

