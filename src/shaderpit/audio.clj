(ns shaderpit.audio
  (:require [jb.vector2 :as v2]
            [shaderpit.util :as util]
            [quil.core :as q]
            )
  (:import (ddf.minim Minim AudioInput))
  (:import (ddf.minim.analysis FFT BeatDetect)))

; Provides audio device input with analysis signals
; for input to visualizations:
; - Input RMS (VU meter) with adjustable smoothing parameter
; - FFT with adjustable smoothing parameter)
; - Beat detection
; - TODO: Waveform

(def ^:dynamic minim)
(def input (atom nil))
(def fft (atom [nil nil]))
(def beat (atom nil))

(def current-state (atom nil))
(def fft-tex (atom nil))


(def default-config {
  :input-level 1.0
  :rms-smooth 0.333
  :fft {
    :size 256
    :style :linear ; :linear | :log-avg
    :smooth 0.333
  }
  :beat-detect {
    :t-debounce-kick 375000000
    :t-debounce-snare 375000000
    :t-debounce-hat 50000000
  }
})

(def default-state {
  :processing? false
  :config default-config
  :audio-buffer nil
  :rms [0.0 0.0]
  :fft-num-bands 0
  :fft [[][]]
  :fft-smooth [[][]]
  :beat-kick 0.0
  :beat-snare 0.0
  :beat-hat 0.0
  :t-kick 0
  :t-snare 0
  :t-hat 0
})


(defn start [state]
  (let [;state @current-state
        config (get state :config default-config)
        fft-size (get-in config [:fft :size])]
    (reset! input (.getLineIn minim Minim/STEREO fft-size))
    (reset! beat (new BeatDetect fft-size (.sampleRate @input)))
    (reset! fft [(new FFT (.bufferSize @input) (.sampleRate @input))
                 (new FFT (.bufferSize @input) (.sampleRate @input))])
    (when (= (config :fft-style) :linear)
      (.noAverages (@fft 0))
      (.noAverages (@fft 1)))
    (when (= (config :fft-style) :log-avg)
      (.logAverages (@fft 0) 55 24)
      (.logAverages (@fft 1) 55 24))

    (let [newstate (-> state
                   (assoc :processing? true)
                   (assoc :fft-num-bands
                     (if (= (get-in config [:fft :style]) :linear)
                       (- (.specSize (@fft 0)) 1 )
                       (.avgSize (@fft 0)))))]
      (println "buffer size" (.bufferSize @input))
      (println "spec-size0" (.specSize (@fft 0)))
      (println "spec-size1" (.specSize (@fft 1)))
      (println "num bands" (newstate :fft-num-bands))
      (reset! fft-tex (q/create-image (newstate :fft-num-bands) 1 :rgb))
      newstate)))


(defn stop [] )


(defn init [parent]
  (def minim (new Minim parent))
  (reset! current-state default-state)
  (swap! current-state start)
)

;(defn set-input-level [level]
;  (reset! input-level level)
;  (.amp input level))



(defn get-rms []
  (@current-state :rms))


(defn get-kick []
  (@current-state :beat-kick))


(defn get-snare []
  (@current-state :beat-snare))


(defn get-hat []
  (@current-state :beat-hat))


(defn- update-rms [state]
  (let [smooth (get-in state [:config :rms-smooth])
        newrms [(.level (.left @input))
                (.level (.right @input))]
        oldsum (state :rms)
        newsum (v2/add oldsum (v2/scale (v2/sub newrms oldsum) smooth))]
    (assoc state :rms newsum)))

;(swap! rms-sum #(+ % (* (- (.analyze rms) %) @rms-smooth))


; get or set the RMS smoothing factor
(defn config-rms-smooth [& factor]
  (when factor
    (swap! current-state assoc-in [:config :rms-smooth] (first factor)))
  (get-in @current-state [:config :rms-smooth]))


(defn config-fft-smooth [& factor]
  (when factor
    (swap! current-state assoc-in [:config :fft :smooth] (first factor)))
  (get-in @current-state [:config :fft :smooth]))


(defn fft-linear [buffer]
  (.forward (@fft 0) (buffer 0))
  (.forward (@fft 1) (buffer 1))
  [(into [] (map #(.getBand (@fft 0) %)
                 (range (.specSize (@fft 0)))))
   (into [] (map #(.getBand (@fft 1) %)
                 (range (.specSize (@fft 1)))))])


(defn get-fft-log [buffer]
  (.forward (@fft 0) buffer)
  (.forward (@fft 1) buffer)
  [(into [] (map #(.getAvg (@fft 0) %)
                 (range (.avgSize (@fft 0)))))
   (into [] (map #(.getAvg (@fft 1) %)
                 (range (.avgSize (@fft 1)))))])


(defn perceptual-scale [band mag nbands]
  (let [rolloff 0.01]
    (* 1.0 (* mag (+ rolloff (* (- 1.0 rolloff)
                                (Math/pow (/ band nbands) 0.6)))))))


(defn get-fft-scaled []
  (let [nbands (@current-state :fft-num-bands)
        left ((@current-state :fft) 0)
        right ((@current-state :fft) 1)]
    [(into [] (map-indexed #(perceptual-scale %1 %2 nbands) left))
     (into [] (map-indexed #(perceptual-scale %1 %2 nbands) right))]))


(defn smooth [oldval newval s]
  (+ oldval (* (- newval oldval) s)))


(defn- update-fft [state]
  (let [buffer [(.toArray (.left @input))
                (.toArray (.right @input))]]
    (assoc state :fft (fft-linear buffer))))


(defn- update-fft-smooth [state]
  (let [;fft (state :fft)
        fft (get-fft-scaled)
        ffts (state :fft-smooth)
        ffts-l (ffts 0)
        ffts-r (ffts 1)
        s (get-in state [:config :fft :smooth])
        left  (into [] (map-indexed #(smooth (get ffts-l %1 0) %2 s) (fft 0)))
        right (into [] (map-indexed #(smooth (get ffts-r %1 0) %2 s) (fft 1)))]
    (assoc state :fft-smooth [left right])))


(defn- update-fft-tex [state]
  (let [texsize (.width @fft-tex)
        xscale (/ (float (state :fft-num-bands)) texsize)
        ;fft (get-fft-scaled)
        fft (state :fft-smooth)
        spectrum-l (fft 0)
        spectrum-r (fft 1)
        ;spectrum-l ((state :fft) 0)
        ;spectrum-r ((state :fft) 1)
        ]
    (doseq [i (range texsize)]
      (let [ix (int (* i xscale))
            intensity-l (* (spectrum-l ix) 255.0)
            intensity-r (* (spectrum-r ix) 255.0)
            col (q/color intensity-l intensity-r 0)]
        (q/set-pixel @fft-tex i 0 col)))
    (q/update-pixels @fft-tex)
    state))


(defn- beat-detect [state]
  (.detect @beat (.mix @input))
  (let [t (System/nanoTime)
        c (state :config)]
    (letfn [
      (detect-kick [s t]
        (if (and (.isKick @beat)
                 (> (- t (s :t-kick)) (get-in c [:beat-detect :t-debounce-kick])))
          (-> s
              (assoc :beat-kick 1.0)
              (assoc :t-kick t))
          (-> s
              (update-in [:beat-kick] #(* % 0.9)))))
      (detect-snare [s t]
        (if (and (.isSnare @beat)
                 (> (- t (s :t-snare)) (get-in c [:beat-detect :t-debounce-snare])))
          (-> s
              (assoc :beat-snare 1.0)
              (assoc :t-snare t))
          (-> s
              (update-in [:beat-snare] #(* % 0.9)))))
      (detect-hat [s t]
        (if (and (.isHat @beat)
                 (> (- t (s :t-hat)) (get-in c [:beat-detect :t-debounce-hat])))
          (-> s
              (assoc :beat-hat 1.0)
              (assoc :t-hat t))
          (-> s
              (update-in [:beat-hat] #(* % 0.9))))) ]
      (-> state
          (detect-kick t)
          (detect-snare t)
          (detect-hat t)))))


(defn process []
  (swap! current-state
         #(-> %
              (update-rms)
              (update-fft)
              (update-fft-smooth)
              ;(update-fft-tex)
              (beat-detect)
              ))
  (update-fft-tex @current-state)
  )

