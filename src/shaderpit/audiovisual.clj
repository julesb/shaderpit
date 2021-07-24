(ns shaderpit.audiovisual
  (:require [quil.core :as q]
            [shaderpit.audio :as audio]))

(def gr (atom nil))
(def fft-tex (atom nil))


(defn init []
  (reset! fft-tex (q/create-image audio/fft-bands 1 :rgb)))


(defn update-fft-tex []
  (let [texsize (.width @fft-tex)
        xscale (/ audio/fft-bands texsize)
        spectrum (audio/get-fft-smooth)]
    (doseq [i (range texsize)]
      (let [ix (int (* i xscale))
            intensity (* (spectrum ix) 255.0)
            col (q/color intensity intensity intensity)]
        (q/set-pixel @fft-tex i 0 col)))
    (q/update-pixels @fft-tex)
    @fft-tex))


(defn draw-meter [x y w h rms]
  (let [rmss (* rms h 6)
        mh (min rmss h)
        my (- (+ y h) mh )
        col (if (> rmss h) [255 0 0 128] [0 255 0 128]) ]
    (q/no-fill)
    (q/stroke-weight 1)
    (q/stroke 128 128 128 128)
    (q/rect x y w h)
    (apply q/fill col)
    (q/no-stroke)
    (q/rect (+ x 1) my (- w 1) mh)))


(defn draw-input-level [x y w h rms]
  (let [margin 4
        mwidth (- (/ w 2) (* margin 2))
        x1 (+ x margin)
        x2 (+ x (/ w 2) margin) ]
    (draw-meter x1 y mwidth h (rms 0))
    (draw-meter x2 y mwidth h (rms 1))))


(defn draw-fft-plot [x y w h]
  (let [px (q/pixels @fft-tex)
        texsize (.width @fft-tex)
        s (/ texsize w)]
    (q/no-fill)
    (q/stroke-weight 1.0)
    (q/stroke 128 128 128 128)
    (q/rect x y w h)
    (q/stroke-weight (/ 0.5 s))
    (q/stroke 255 255 255 255)
    (q/begin-shape :points)
    (doseq [i (range texsize)]
      (let [idx (int (* i ))
            x1 (aget px idx)
            sy (/ (q/red x1) 255.0) ]
        (q/vertex (+ x (/ i s))
                  (- (+ y h) (* sy (- h 3))))))
    (q/end-shape)))


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


(defn draw-beats [x y w]
  (q/fill 0 0 0 192)
  (q/rect (- x (/ w 2)) (- y (/ w 2)) (* w 5) w (/ w 2))
  (q/fill 255 0 0 (* @audio/beat-kick 255))
  (q/ellipse (+ x (* 0 w)) y w w)
  (q/fill 255 255 0 (* @audio/beat-snare 255))
  (q/ellipse (+ x (* 2 w)) y w w)
  (q/fill 0 0 255 (* @audio/beat-hat 255))
  (q/ellipse (+ x (* 4 w)) y w w))


