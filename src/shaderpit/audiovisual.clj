(ns shaderpit.audiovisual
  (:require [quil.core :as q]
            [shaderpit.audio :as audio]))

(def gr (atom nil))
(def fft-tex (atom nil))


(defn init []
  (reset! fft-tex (q/create-image audio/fft-bands 1 :rgb)))


(defn update-fft-tex [spectrum]
  (let [texsize (.width @fft-tex)
        xscale (/ (count spectrum) texsize) ]
  (doseq [i (range texsize)]
    (let [ix (int (* i xscale))
          intensity (* (spectrum ix) 255.0 40.0)
          col (q/color intensity intensity intensity)]
      (q/set-pixel @fft-tex i 0 col)))
  (q/update-pixels @fft-tex)
  @fft-tex))


(defn draw-input-level [x y w h rms]
  (let [mheight (* rms h 2)
        my (- (+ y h) mheight) ]
    (q/no-fill)
    (q/stroke 128 128 128 128)
    (q/rect x y w h)
    (q/fill 0 255 0 128)
    (q/rect (+ x 1) my (- w 2) mheight)))


(defn draw-fft [x y w h]
  (let [px (q/pixels @fft-tex)
        texsize (.width @fft-tex)
        s (/ texsize w)]
    (q/no-fill)
    (q/stroke-weight 1.0)
    (q/stroke 128 128 128 128)
    (q/rect x y w h)
    (q/stroke 255 255 255 255)
    (doseq [i (range texsize)]
      (let [idx (int (* i ))
            c (aget px idx)
            sy (/ (q/red c) 255.0) ]
        (q/line (+ x (/ i s))
                (+ y h -1)
                (+ x (/ i s))
                (- (+ y h) (* sy h)))))))


