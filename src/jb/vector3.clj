(ns jb.vector3
  (:refer-clojure :exclude [int max min format]))


(defn add ^doubles [^doubles [v1x v1y v1z] ^doubles [v2x v2y v2z]]
  [(+ v1x v2x) (+ v1y v2y) (+ v1z v2z)])

;(defn add [v1 v2]
;  [(+ (v1 0) (v2 0))
;   (+ (v1 1) (v2 1))
;   (+ (v1 2) (v2 2))])

;(defn add [v1 v2]
;  (vec (map #(+ %1 %2) v1 v2)))

(defn sub ^doubles [^doubles v1 ^doubles v2]
  [(- (v1 0) (v2 0))
   (- (v1 1) (v2 1))
   (- (v1 2) (v2 2))])

(defn mul ^doubles [^doubles v1 ^doubles v2]
  [(* (v1 0) (v2 0))
   (* (v1 1) (v2 1))
   (* (v1 2) (v2 2))])

(defn div ^doubles [^doubles v1 ^doubles v2]
  [(/ (v1 0) (v2 0))
   (/ (v1 1) (v2 1))
   (/ (v1 2) (v2 2))])

(defn scale ^doubles [^doubles v ^double s]
  [(* (v 0) s) (* (v 1) s) (* (v 2) s)])

(defn sum-of-squares ^double [^doubles v]
  (+ (* (v 0) (v 0)) (* (v 1) (v 1)) (* (v 2) (v 2))))

;(defn sum-of-squares [[x y z]]
;  (+ (* x x) (* y y) (* z z)))

(defn length ^double [^doubles [x y z]]
  (Math/sqrt (+ (* x x) (* y y) (* z z))))

(defn normalize ^doubles [^doubles [x y z]]
  (let [l (length [x y z])]
     (if (zero? l)
        [(/ x 0.000000001) (/ y 0.000000001) (/ z 0.000000001)]
        [(/ x l) (/ y l) (/ z l)])))

(defn cross [[v1x v1y v1z] [v2x v2y v2z]]
  [ (- (* v1y v2z) (* v1z v2y))
    (- (* v1z v2x) (* v1x v2z))
    (- (* v1x v2y) (* v1y v2x))])

(defn dot [[v1x v1y v1z] [v2x v2y v2z]]
  (+ (* v1x v2x) (* v1y v2y) (* v1z v2z)))

(defn angle-between [v1 v2]
  (let [l1 (length v1)
        l2 (length v2)
        dp (dot v1 v2)
        rads (Math/acos (/ dp (* l1 l2)))
        degs (* (/ rads Math/PI) 180.0) ]
    degs))

(defn distance [p1 p2]
  (length (sub p2 p1)))

(defn distance-squared [p1 p2]
  (sum-of-squares (sub p2 p1)))

(defn bisect [p1 p2]
  (scale (add p1 p2) 0.5))

(defn abs ^doubles [^doubles v] [(Math/abs (v 0)) (Math/abs (v 1)) (Math/abs (v 2))])

(defn max [v1 v2] [(max (v1 0) (v2 0)) (max (v1 1) (v2 1)) (max (v1 2) (v2 2))])

(defn min [v1 v2] [(min (v1 0) (v2 0)) (min (v1 1) (v2 1)) (min (v1 2) (v2 2))])

(defn clipmag [v maxmag]
  (if (< (sum-of-squares v) (* maxmag maxmag))
    v
    (scale (normalize v) maxmag)))


(defn equal? [v1 v2]
  (and (= (v1 0) (v2 0))
       (= (v1 1) (v2 1))
       (= (v1 2) (v2 2))))


(defn int [v]
  [(clojure.core/int (v 0))
   (clojure.core/int (v 1))
   (clojure.core/int (v 2))])

(defn myround [s n]
  (double (.setScale (bigdec n) s java.math.RoundingMode/HALF_EVEN)))


(defn eps-to-zero [n e]
  (if (< (Math/abs n) e) 0.0 n))

(defn eps-to-zero [v e]
  [(eps-to-zero (v 0) e)
   (eps-to-zero (v 1) e)
   (eps-to-zero (v 2) e)])


;(defn quantize [v p]
;  [(myround p (v 0))
;   (myround p (v 1))
;   (myround p (v 2))])

(def quantize (memoize (fn [v p]
  [(myround p (v 0))
   (myround p (v 1))
   (myround p (v 2))])))

(defn rotate-point [point axis ang]
  (if (= ang 0.0)
    point
    (let [[x y z] point
          [u v w] axis
          ux (* u x) uy (* u y) uz (* u z)
          vx (* v x) vy (* v y) vz (* v z)
          wx (* w x) wy (* w y) wz (* w z)
          sa (Math/sin ang)
          ca (Math/cos ang)
          xn (+ (* u (+ ux vy wz))
                (* ca (- (* x (+ (* v v) (* w w)))
                         (* u (+ vy wz))))
                (* sa (+ (- wy) vz)))
          yn (+ (* v (+ ux vy wz))
                (* ca (- (* y (+ (* u u) (* w w)))
                         (* v (+ ux wz))))
                (* sa (- wx uz)))
          zn (+ (* w (+ ux vy wz))
                (* ca (- (* z (+ (* u u) (* v v)))
                         (* w (+ ux vy))))
                (* sa (+ (- vx) uy))) ]
      [xn yn zn])))



; p=point, pv=point on plane, pn=plane normal
(defn project-point-to-plane [p pv pn]
  (let [;norm (fn [v] (Math/sqrt (dot v v)))
        ;dist (fn [p q] (norm (sub p q)))
        sn (- (dot pn (sub p pv)))
        sd (dot pn pn)
        sb (/ sn sd)
        b (add p (scale pn sb))]
    b))

;; p=point, pv=point on plane, pn=plane normal
(defn distance-point-to-plane [p pv pn]
  (distance p (project-point-to-plane p pv pn)))


(defn signed-distance-point-to-plane [p pv pn]
  (let[prj-p (project-point-to-plane p pv pn)
       dir (sub p prj-p)
       n (normalize dir)
       dist (length dir)]
    (if (>= (dot n (normalize p)) 0.0)
      dist
      (- dist))))


(defn format [v]
  (if (and (not= nil v) (>= (count v) 3))
    (clojure.core/format "[%.2f %.2f %.2f]" (v 0) (v 1) (v 2))
    ""))

;; p=point, pv=point on plane, pn=plane normal
;(defn distance-point-to-plane [p pv pn]
;  (let [norm (fn [v] (Math/sqrt (dot v v)))
;        dist (fn [p q] (norm (sub p q)))
;        sn (- (dot pn (sub p pv)))
;        sd (dot pn pn)
;        sb (/ sn sd)
;        b (add p (scale pn sb))]
;    (dist p b)))


