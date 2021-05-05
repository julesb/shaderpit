(ns shaderpit.console
  (:require [quil.core :as q]))

(def line-buffer (atom ()))
(def ctx (atom {:width 800
                :height 400
                :gr nil
                :dirty true}))

(def max-lines 8)
(def ^:dynamic font)

(defn init [dim]
  ;(def font (q/load-font "data/FreeMono-16.vlw"))
  (def font (q/load-font "data/AmericanTypewriter-24.vlw"))
  (when (@ctx :gr)
    (.dispose (@ctx :gr)))
  (reset! ctx {:gr (q/create-graphics (dim 0) (dim  1) :p2d)
               :dirty true
               :width (dim 0)
               :height (dim 1)})
  (reset! line-buffer ()))


(defn writeln [s]
  (dosync
    (swap! ctx assoc :dirty true)
    (swap! line-buffer conj s)
    (when (> (count @line-buffer) max-lines)
      (swap! line-buffer #(take max-lines %)))))


(defn render []
  (q/with-graphics (@ctx :gr)
    (q/text-font font)
    (q/background 0 0 0 64)
    (let [x 4 y 20
          line-space 25
          lines (vec @line-buffer)]
      (q/fill 255)
      (doseq [i (range (count lines))]
        (let [idx (- (dec (count lines)) i)
              fade (Math/sqrt (double (/ idx max-lines)))
              alpha (- 255 (int (* 255.0 fade)))]

          ;(q/fill 255 255 255 alpha)
          (q/text (format "[%d] %s" alpha (lines idx))
                  x (+ y (* i line-space)))))
          ;(q/text (lines idx) x (+ y (* i line-space)))))
      (when (= lines @line-buffer)
        (swap! ctx assoc :dirty false)))))


(defn update []
  (when (@ctx :dirty)
    (render)))


(defn get-image []
  (@ctx :gr))
