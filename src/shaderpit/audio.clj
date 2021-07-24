(ns shaderpit.audio
  (:require [jb.vector2 :as v2]
            [shaderpit.util :as util]
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
(def ^:dynamic input)
(def ^:dynamic fft)
(def ^:dynamic beat)
(def ^:dynamic fft-bands)
(def ^:dynamic fft-spectrum)
(def ^:dynamic rms)

(def ^:const fft-style :linear) ; :linear | :log-avg
(def ^:const fft-size 256)

(def fft-smooth (atom 0.333)) ; FFT smooth factor 1 = no smoothing
(def rms-smooth (atom 0.333)) ; RMS smooth factor 1 = no smoothing
(def rms-sum (atom [0.0 0.0]))     ; RMS running avg
(def input-level (atom 1.0)) ; Input signal attenuation
(def fft-smooth-vals (atom []))

(def beat-kick (atom 0.0))
(def beat-snare (atom 0.0))
(def beat-hat (atom 0.0))
(def t-kick (atom 0))
(def t-snare (atom 0))
(def t-hat (atom 0))
(def t-debounce-kick (atom 50000000))
(def t-debounce-snare (atom 50000000))
(def t-debounce-hat (atom 50000000))


(defn init [parent]
  (def minim (new Minim parent))
  (def input (.getLineIn minim Minim/STEREO fft-size))
  (def beat (new BeatDetect fft-size (.sampleRate input)))
  (def fft (new FFT (.bufferSize input) (.sampleRate input)))
  (if (= fft-style :linear)
    (do
      (.noAverages fft)
      (def fft-bands (- (.specSize fft) 1 )))
    (do
      (.logAverages fft 55 24)
      (def fft-bands (.avgSize fft))))

  (println "FFT bands:" fft-bands))


(defn start [] )

(defn stop [] )


;(defn set-input-level [level]
;  (reset! input-level level)
;  (.amp input level))


; update the running average rms and return it
(defn get-input-rms []
  (let [newrms [(.level (. input left)) (.level (. input right))]
        newsum (v2/add @rms-sum (v2/scale (v2/sub newrms @rms-sum) @rms-smooth))]
    (reset! rms-sum newsum)))


;(swap! rms-sum #(+ % (* (- (.analyze rms) %) @rms-smooth))


; get or set the RMS smoothing factor
(defn rms-smooth-factor [& factor]
  (when factor
    (reset! rms-smooth (first factor)))
  @rms-smooth)


(defn fft-forward []
  (.forward fft (. input mix)))


(defn get-fft []
  (.forward fft (. input mix))
  (if (= fft-style :linear)
    (into [] (map #(.getBand fft %) (range (.specSize fft))))
    ;(into [] (take fft-bands (.getSpectrumReal fft)))
    (into [] (map #(.getAvg fft %) (range (.avgSize fft)))))
  )

(defn perceptual-scale [band mag]
  (let [rolloff 0.01]
    (* 1.0 (* mag (+ rolloff (* (- 1.0 rolloff)
                                (Math/pow (/ band fft-bands) 0.6)))))))


(defn get-fft-scaled []
  (->> (get-fft)
       (map-indexed perceptual-scale)
       (into [])))


(defn smooth [idx newval]
  (+ (get @fft-smooth-vals idx 0.0)
      (* (- newval (get @fft-smooth-vals idx 0.0))
         @fft-smooth)))


(defn get-fft-smooth []
  (let [spectrum (get-fft-scaled)]
    (reset! fft-smooth-vals (into [] (map-indexed smooth spectrum)))) )


(defn beat-detect []
  (.detect beat (. input mix))
  (let [t (System/nanoTime)]
    (if (and (.isKick beat) (> (- t @t-kick) @t-debounce-kick))
      (do (reset! beat-kick 1.0)
          (reset! t-kick t))
      (swap! beat-kick #(* % 0.9)))
    (if (and (.isSnare beat) (> (- t @t-snare) @t-debounce-snare))
      (do (reset! beat-snare 1.0)
          (reset! t-snare t))
      (swap! beat-snare #(* % 0.9)))
    (if (and (.isHat beat) (> (- t @t-hat) @t-debounce-hat ))
      (do (reset! beat-hat 1.0)
          (reset! t-hat t))
      (swap! beat-hat #(* % 0.9)))))


(defn process []
  ; TODO update fft state here
  (beat-detect))

