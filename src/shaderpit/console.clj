(ns shaderpit.console
  (:require [quil.core :as q]
            [clojure.string :as str]))

(def line-buffer (atom ()))
(def ctx (atom {:width 800
                :height 400
                :gr nil
                :dirty true}))

(def size [920 200])
(def max-lines 8)
(def wrap-chars 80)
(def ^:dynamic font)
(def ^:const promptstr "=> ")


(defn writeln [s]
  (println promptstr s)
  (swap! ctx assoc :dirty true)
  (let [lines (reverse (map (partial apply str)
                            (partition-all wrap-chars (str promptstr s))))]
    (swap! line-buffer #(->> % (concat lines) (take max-lines )))))


(defn writeseq [ss]
  (when (> (count ss) 0)
    (doseq [s ss]
      (writeln s))))


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


(defn init []
  (def font (q/load-font "data/app/fonts/AmericanTypewriter-24.vlw"))
  (when (@ctx :gr)
    (.dispose (@ctx :gr)))
  (reset! ctx {:gr (q/create-graphics (size 0) (size 1) :p2d)
               :dirty true
               :width (size 0)
               :height (size 1)})
  (reset! line-buffer ())
  (render))



(defn update! []
  (when (@ctx :dirty)
    (render)))


(defn get-image []
  (@ctx :gr))
