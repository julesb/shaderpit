(ns shaderpit.audio
  (:import (processing.sound AudioIn Amplitude FFT Waveform BeatDetector)))

; Provides audio device input with analysis signals
; for input to visualizations:
; - Input RMS (VU meter) with adjustable smoothing parameter
; - FFT (TODO: adjustable smoothing parameter)
; - TODO: Beat detection
; - TODO: Waveform


(def ^:dynamic input)
;(def ^:dynamic applet-ref)
(def ^:dynamic fft)
(def ^:dynamic fft-bands)
(def ^:dynamic fft-spectrum)
(def ^:dynamic rms)

(def rms-smooth (atom 0.25)) ; RMS smooth factor 1 = no smoothing
(def rms-sum (atom 0.0))     ; RMS running avg
(def input-level (atom 1.0)) ; Input signal attenuation


(defn init [parent device _fft-bands]
  (def input (new AudioIn parent device))
  (def rms (new Amplitude parent))
  (def fft-bands _fft-bands)
  (def fft (new FFT parent fft-bands))
  (def fft-spectrum (float-array fft-bands, 0.0))
  (.input rms input) ; patch the input to amplitude analyzer
  (.input fft input) ; patch the input to FFT analyzer
  )

(defn start [] (.start input))
(defn stop [] (.stop input))


(defn set-input-level [level]
  (reset! input-level level)
  (.amp input level))


; update the running average rms and return it
(defn get-input-rms []
  (swap! rms-sum #(+ % (* (- (.analyze rms) %) @rms-smooth))))


; get or set the RMS smoothing factor
(defn rms-smooth-factor [& factor]
  (when factor
    (reset! rms-smooth factor))
  @rms-smooth)


(defn get-fft []
  (.analyze fft fft-spectrum)
  (into [] fft-spectrum))


(defn perceptual-scale [band mag]
  (* (Math/sqrt (Math/log10 (+ 1.0 (* mag mag))))
     (Math/sqrt (/ band fft-bands)))) ; ouch!


(defn get-fft-scaled []
  (->> (get-fft)
       (map-indexed perceptual-scale)
       (into [])))


