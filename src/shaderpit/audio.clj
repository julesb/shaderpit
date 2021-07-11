(ns shaderpit.audio
  (:import (processing.sound
             Amplitude
             AudioIn
             BeatDetector
             FFT
             Waveform
             )))

; Provides audio device input with analysis signals
; for input to visualizations:
; - Input RMS (VU meter) with adjustable smoothing parameter
; - FFT (TODO: adjustable smoothing parameter)
; - TODO: Beat detection
; - TODO: Waveform


(def ^:dynamic input)
(def ^:dynamic fft)
(def ^:dynamic fft-bands)
(def ^:dynamic fft-spectrum)
(def ^:dynamic rms)

(def fft-smooth (atom 1.0)) ; FFT smooth factor 1 = no smoothing
(def rms-smooth (atom 1.0)) ; RMS smooth factor 1 = no smoothing
(def rms-sum (atom 0.0))     ; RMS running avg
(def input-level (atom 1.0)) ; Input signal attenuation
(def fft-smooth-vals (atom []))

(defn init [parent device _fft-bands]
  (def input (new AudioIn parent device))
  (def rms (new Amplitude parent))
  (def fft-bands _fft-bands)
  (def fft (new FFT parent fft-bands))
  (def fft-spectrum (float-array fft-bands, 0.0))
  (.input rms input) ; patch the input to amplitude analyzer
  (.input fft input) ; patch the input to FFT analyzer  )
  (.amp input @input-level)
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
    (reset! rms-smooth (first factor)))
  @rms-smooth)




(defn get-fft [downscale]
  (.analyze fft fft-spectrum)
  (into [] (take (/ fft-bands downscale) fft-spectrum)))


(defn perceptual-scale [band mag]
  (let [rolloff 0.01]
    (* 4.0 (* mag (+ rolloff (* (- 1.0 rolloff)
                                (Math/pow (/ band fft-bands) 0.6)))))))


(defn get-fft-scaled [downscale]
  (->> (get-fft downscale)
       (map-indexed perceptual-scale)
       (into [])))


(defn smooth [idx newval]
  (+ (get @fft-smooth-vals idx 0.0)
      (* (- newval (get @fft-smooth-vals idx 0.0))
         @fft-smooth)))


(defn get-fft-smooth []
  (let [spectrum (get-fft-scaled 1)]
    (reset! fft-smooth-vals (into [] (map-indexed smooth spectrum)))) )


