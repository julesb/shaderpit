(ns shaderpit.console
  (:require [quil.core :as q]
            [clojure.string :as str]))

(def line-buffer (atom ()))
(def ctx (atom {:width 800
                :height 400
                :gr nil
                :dirty true}))

(def max-lines 8)
(def wrap-chars 64)
(def ^:dynamic font)
(def ^:const promptstr "=> ")

(defn init [dim]
  ;(def font (q/load-font "data/FreeMono-16.vlw"))
  (def font (q/load-font "data/app/fonts/AmericanTypewriter-24.vlw"))
  (when (@ctx :gr)
    (.dispose (@ctx :gr)))
  (reset! ctx {:gr (q/create-graphics (dim 0) (dim  1) :p2d)
               :dirty true
               :width (dim 0)
               :height (dim 1)})
  (reset! line-buffer ()))


(defn writeln [s]
  (swap! ctx assoc :dirty true)
  (let [lines (reverse (map (partial apply str)
                            (partition-all wrap-chars (str promptstr s))))]
    (swap! line-buffer #(->> % (concat lines) (take max-lines )))))


(defn render []
  (q/with-graphics (@ctx :gr)
    (q/text-font font)
    (q/background 0 0 0 64)
    (let [x 10 y 20
          line-space 25
          lines (vec @line-buffer)
          maxlineidx (dec (count lines))
          ;highlight (fn [i, line] (if (= i maxlineidx) 255 160)) 
          ]
      (doseq [i (range (count lines))]
        (let [idx (- (dec (count lines)) i)
              fade (Math/sqrt (double (/ idx max-lines)))
              alpha (if (= i maxlineidx) 255 160)
              ;alpha (- 255 (int (* 255.0 fade)))
              ]
          (q/fill 255 255 255 alpha)
          (q/text (lines idx) x (+ y (* i line-space)))))
      (when (= lines @line-buffer)
        (swap! ctx assoc :dirty false)))))


(defn update! []
  (when (@ctx :dirty)
    (render)))


(defn get-image []
  (@ctx :gr))
