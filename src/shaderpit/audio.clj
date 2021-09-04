(ns shaderpit.audio
  (:require [jb.vector2 :as v2]
            [shaderpit.util :as util]
            [quil.core :as q]
            )
  (:import (ddf.minim Minim AudioInput AudioListener))
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
(def fft-raw-tex (atom nil))


(def default-config {
  :input-level 1.0
  :rms-smooth 0.333
  :fft {
    :size 1024
    :style :linear ; :linear | :log-avg
    :smooth 0.2
    :window :hamming
  }
  :beat-detect {
    :t-debounce-kick 375000000
    :t-debounce-snare 375000000
    :t-debounce-hat 50000000
  }
  :energy-history-size 43
})

(def default-state {
  :processing? false
  :config default-config
  :buffer nil
  :rms [0.0 0.0]
  :fft-num-bands 0
  :fft [[][]]
  :fft-smooth [[][]]
  :fft-raw [[[][]][[][]]]
  :variance [0.0 0.0]
  :energy [0.0 0.0]
  :energy-history [[0 0] [0 0]]
  :energy-history-avg [0.0 0.0]
  :beat-kick 0.0
  :beat-snare 0.0
  :beat-hat 0.0
  :t-kick 0
  :t-snare 0
  :t-hat 0
})



;(defn set-input-level [level]
;  (reset! input-level level)
;  (.amp input level))



(defn get-rms []
  (@current-state :rms))


(defn get-db []
  (let [rms (@current-state :rms)]
    [(+ 100.0 (* (/ 20.0 2.302585092994) (Math/log (rms 0))))
     (+ 100.0 (* (/ 20.0 2.302585092994) (Math/log (rms 1))))]))


(defn get-kick []
  (@current-state :beat-kick))


(defn get-snare []
  (@current-state :beat-snare))


(defn get-hat []
  (@current-state :beat-hat))


(defn get-energy []
  ;(@current-state :energy))
  (@current-state :energy-history-avg))


(defn get-variance []
  (@current-state :variance))


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


(defn config-energy-history-size [& size]
  (when size
    (swap! current-state assoc-in [:config :energy-history-size](first size)))
  (get-in @current-state [:config :energy-history-size]))


(defn fft-set-window [w]
  (let [windows {
          :bartlett FFT/BARTLETT
          :blackman FFT/BLACKMAN
          :cosine FFT/COSINE
          :gauss FFT/GAUSS
          :hamming FFT/HAMMING
          :hann FFT/HANN
          :lanczos FFT/LANCZOS
          :none FFT/NONE
          :triangular FFT/TRIANGULAR
        }
        window (get windows w FFT/NONE)]
    (swap! current-state assoc-in [:config :fft :window] w)
    (.window (@fft 0) window)
    (.window (@fft 1) window)))


(defn fft-linear [buffer]
  (.forward (@fft 0) (buffer 0))
  (.forward (@fft 1) (buffer 1))
  [(vec (map #(.getBand (@fft 0) %)
             (range (.specSize (@fft 0)))))
   (vec (map #(.getBand (@fft 1) %)
             (range (.specSize (@fft 1)))))])


(defn fft-log [buffer]
  (.forward (@fft 0) (buffer 0))
  (.forward (@fft 1) (buffer 1))
  [(vec (map #(.getAvg (@fft 0) %)
             (range (.avgSize (@fft 0)))))
   (vec (map #(.getAvg (@fft 1) %)
             (range (.avgSize (@fft 1)))))])


(defn perceptual-scale [band mag nbands rolloff]
  (* mag (+ rolloff (* (- 1.0 rolloff)
                       (Math/pow (/ band nbands) 0.6)))))


(defn get-fft-scaled [state]
  (let [nbands (state :fft-num-bands)
        rolloff 0.01
        [l r] (state :fft)]
    [(vec (map-indexed #(perceptual-scale %1 %2 nbands rolloff) l))
     (vec (map-indexed #(perceptual-scale %1 %2 nbands rolloff) r))]))


(defn smooth [oldval newval s]
  (+ oldval (* (- newval oldval) s)))


(defn- update-fft [state]
  (if (= :linear (get-in state [:config :fft :style]))
    (assoc state :fft (fft-linear (state :buffer)))
    (assoc state :fft (fft-log (state :buffer)))))


(defn- update-fft-raw [state]
  (let [tx #(vec (take (/ (count %) 2) %))
        l [(tx (.getSpectrumReal (@fft 0)))
           (tx (.getSpectrumImaginary (@fft 0)))]
        r [(tx (.getSpectrumReal (@fft 1)))
           (tx (.getSpectrumImaginary (@fft 1)))] ]
    (assoc state :fft-raw [l r])))


(defn- update-fft-smooth [state]
  (let [fft (get-fft-scaled state)
        [ffts-l ffts-r] (state :fft-smooth)
        s (get-in state [:config :fft :smooth])
        left  (vec (map-indexed #(smooth (get ffts-l %1 0) %2 s) (fft 0)))
        right (vec (map-indexed #(smooth (get ffts-r %1 0) %2 s) (fft 1)))]
    (assoc state :fft-smooth [left right])))


(defn- update-fft-tex [state]
  (let [texsize (.width @fft-tex)
        xscale (/ (float (state :fft-num-bands)) texsize)
        ;fft (get-fft-scaled)
        fft (state :fft-smooth)
        spectrum-l (fft 0)
        spectrum-r (fft 1)]
    (doseq [i (range texsize)]
      (let [ix (int (* i xscale))
            intensity-l (* (spectrum-l ix) 255.0)
            intensity-r (* (spectrum-r ix) 255.0)
            intensity-d (+ 0.5 (* 0.5 (- (spectrum-r ix)
                                         (spectrum-l ix))))
            intensity-d (* intensity-d 255.0)
            col (q/color intensity-l intensity-r intensity-d)]
        (q/set-pixel @fft-tex i 0 col)))
    (q/update-pixels @fft-tex)
    state))


(defn- update-fft-raw-tex [state]
  (let [texsize (.width @fft-raw-tex)
        xscale (/ (float (state :fft-num-bands)) texsize)
        [[lr li] [rr ri]] (state :fft-raw) ]
    (doseq [i (range texsize)]
      (let [ix (int (* i xscale))
            txr #(+ 127.0 (* % 128.0 0.5))
            txi #(+ 127.0 (* (/ % Math/PI) 128.0 0.5))
            col (q/color (txr (lr ix))
                         (txi (li ix))
                         (txr (rr ix))
                         (txi (ri ix)))]
        (q/set-pixel @fft-raw-tex i 0 col)))
    (q/update-pixels @fft-raw-tex)
    state))


(defn update-energy [state]
  (let [[buf-l buf-r] (state :buffer)
        histsize (get-in state [:config :energy-history-size])
        en-l (reduce + (map #(* % %) buf-l))
        en-r (reduce + (map #(* % %) buf-r))
        ;en-l (/ (reduce + (map #(* % %) buf-l)) (count buf-l))
        ;en-r (/ (reduce + (map #(* % %) buf-r)) (count buf-r))
        histnew  (->> (state :energy-history)
                      (concat [[en-l en-r]])
                      (take histsize))
        histavg (v2/scale (reduce v2/add (map v2/sqr histnew))
                          (/ 1.0 histsize)) ]
    (-> state
        (assoc :energy [en-l en-r])
        (assoc :energy-history histnew)
        (assoc :energy-history-avg histavg))))


(defn update-variance [state]
  (let [histsize (get-in state [:config :energy-history-size])
        oldv (state :variance)
        newv (v2/scale (reduce v2/add
                               (map #(v2/sqr (v2/sub (state :energy) %))
                                    (state :energy-history)))
                       (/ 1.0 histsize))
        sm 1.0
        smoothv (v2/smooth oldv newv sm)]
    (assoc state :variance smoothv)))


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
  (when (@current-state :processing?)
    (update-fft-tex @current-state)
    (update-fft-raw-tex @current-state)))


(defn audio-buffer-callback []
  (reify AudioListener
    (samples [this sampL sampR]
      (when (and (@current-state :processing?)
                 (not (nil? sampL))
                 (not (nil? sampR)))
        (swap! current-state
               #(-> %
                    (assoc :buffer [sampL sampR])
                    (update-rms)
                    (update-energy)
                    (update-variance)
                    (update-fft)
                    (update-fft-smooth)
                    (update-fft-raw)
                    (beat-detect)))))))


(defn- start-processing [state]
  (let [config (get state :config)
        fft-size (get-in config [:fft :size])
        fft-style (get-in config [:fft :style])
        fft-win (get-in config [:fft :window])]
    (reset! input (.getLineIn minim Minim/STEREO fft-size))
    (reset! beat (new BeatDetect fft-size (.sampleRate @input)))
    (reset! fft [(new FFT (.bufferSize @input) (.sampleRate @input))
                 (new FFT (.bufferSize @input) (.sampleRate @input))])
    ;(fft-set-window :hamming)
    (.window (@fft 0) FFT/HAMMING)
    (.window (@fft 1) FFT/HAMMING)
    (when (= fft-style :linear)
      (.noAverages (@fft 0))
      (.noAverages (@fft 1)))
    (when (= fft-style :log-avg)
      (.logAverages (@fft 0) 22 96)
      (.logAverages (@fft 1) 22 96))

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
      (reset! fft-raw-tex (q/create-image (newstate :fft-num-bands) 1 :argb))
      (.addListener @input (audio-buffer-callback))

      newstate)))


(defn start []
  (swap! current-state assoc :processing? true))


(defn stop []
  (swap! current-state assoc :processing? false))


(defn init [parent]
  (def minim (new Minim parent))
  (reset! current-state default-state)
  (swap! current-state start-processing)
)
