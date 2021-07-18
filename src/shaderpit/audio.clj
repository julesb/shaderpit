(ns shaderpit.audio
  (:import (ddf.minim Minim AudioInput))
  (:import (ddf.minim.analysis FFT)))

; Provides audio device input with analysis signals
; for input to visualizations:
; - Input RMS (VU meter) with adjustable smoothing parameter
; - FFT (TODO: adjustable smoothing parameter)
; - TODO: Beat detection
; - TODO: Waveform

(def ^:dynamic minim)
(def ^:dynamic input)
(def ^:dynamic fft)
(def ^:dynamic fft-bands)
(def ^:dynamic fft-spectrum)
(def ^:dynamic rms)
(def ^:const fft-style :linear) ; :linear | :log-avg

(def fft-smooth (atom 0.333)) ; FFT smooth factor 1 = no smoothing
(def rms-smooth (atom 1.0)) ; RMS smooth factor 1 = no smoothing
(def rms-sum (atom 0.0))     ; RMS running avg
(def input-level (atom 1.0)) ; Input signal attenuation
(def fft-smooth-vals (atom []))
(defn init [parent]
  (def minim (new Minim parent))
  (def input (.getLineIn minim Minim/STEREO 2048))
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
  [(.level (. input left)) (.level (. input right))])


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


