(ns jb.vector2
  (:refer-clojure :exclude [format]))

; 2d vector ops
(defn angle-to-vec2 [angle]
  (let [rad (Math/toRadians angle)]
    [(Math/cos rad)
     (Math/sin rad)]))

(defn add [v1 v2]
  [(+ (v1 0) (v2 0))
   (+ (v1 1) (v2 1))])

(defn sub [v1 v2]
  [(- (v1 0) (v2 0))
   (- (v1 1) (v2 1))])

(defn scale [v s]
  [(* (v 0) s) (* (v 1) s)])

(defn mul [v1 v2]
  [(* (v1 0) (v2 0))
   (* (v1 1) (v2 1))])

(defn div [v1 v2]
  [(/ (v1 0) (v2 0))
   (/ (v1 1) (v2 1))])


(defn sqr [v]
  [(* (v 0) (v 0)) (* (v 1) (v 1))])

(defn sum-of-squares ^double [^doubles v]
  (+ (* (v 0) (v 0)) (* (v 1) (v 1))))

(defn length [[x y]]
  (Math/sqrt (+ (* x x) (* y y))))

(defn distance [p1 p2]
  (length (sub p2 p1)))

(defn distance-squared [p1 p2]
  (sum-of-squares (sub p2 p1)))

(defn normalize [[x y]]
  (let [l (length [x y])]
     (if (zero? l)
        [(/ x 0.000000001) (/ y 0.000000001)]
        [(/ x l) (/ y l)])))

(defn equal? [v1 v2]
  (and (= (v1 0) (v2 0))
       (= (v2 1) (v2 1))))

(defn smooth [oldv newv amount]
  (add oldv (scale (sub newv oldv) amount)))


(defn format [v]
  (clojure.core/format "[%.2f %.2f]"
                       (float (get v 0 0.0))
                       (float (get v 1 0.0))))

; http://paulbourke.net/geometry/lineline2d/"

(defn line-intersection [x1 y1 x2 y2 x3 y3 x4 y4]
  ;(println x1 y1 x2 y2 x3 y3 x4 y4)
  (when (and (not (equal? [x1 y1] [x2 y2]))
             (not (equal? [x3 y3] [x4 y4])))
    (let [ua (/ (- (* (- x4 x3) (- y1 y3))
                   (* (- y4 y3) (- x1 x3)))
                (- (* (- y4 y3) (- x2 x1))
                   (* (- x4 x3) (- y2 y1))))
          ub (/ (- (* (- x2 x1) (- y1 y3))
                   (* (- y2 y1) (- x1 x3)))
                (- (* (- y4 y3) (- x2 x1))
                   (* (- x4 x3) (- y2 y1))))]
    (when (and (>= ua 0.0)
               (<= ua 1.0)
               (>= ub 0.0)
               (<= ub 1.0))
      [(+ x1 (* ua (- x2 x1)))
       (+ y1 (* ua (- y2 y1)))]))))
