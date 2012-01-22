;   Copyright (c) Rich Hickey and contributors.
;   All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;   Alioth benchmarks: http://shootout.alioth.debian.org/u64q/benchmark.php?test=nbody&lang=all
;   Inspired by http://shootout.alioth.debian.org/u64q/program.php?test=nbody&lang=java&id=1

(ns alioth.nbody
  (:gen-class))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defmacro for-loop [[binding pred adv] & body]
  `(loop [~@binding]
     (when ~pred
       ~@body
       (recur ~adv))))

(def ^:const pi 3.141592653589793)
(def ^:const solar-mass (* 4 pi pi))
(def ^:const days-per-year 365.24)

(defmacro defbody [& fields]
  (letfn [(getsym [f] (symbol (str "get" f)))
          (+=sym [f] (symbol (str "+=" f "!")))
          (-=sym [f] (symbol (str "-=" f "!")))
          (tag [f]
            (with-meta f
              {:unsynchronized-mutable true :tag 'double}))
          (getter [f]
            `(~(with-meta (getsym f) {:tag 'double}) []))
          (getter-impl [f]
            `(~(getsym f) [~'this] ~f))
          (setters [f]
            [`(~(+=sym f) [~'this ~'n])
             `(~(-=sym f) [~'this ~'n])])
          (setters-impl [f]
            [`(~(+=sym f) [~'_ ~'n] (~'set! ~f (+ ~f ~'n)))
             `(~(-=sym f) [~'_ ~'n] (~'set! ~f (- ~f ~'n)))])]
    `(do
       (definterface ~'IBodyGet ~@(map getter fields))
       (defprotocol ~'IBodySet ~@(mapcat setters fields))
       (deftype ~'Body [~@(map tag fields)]
         ~'IBodyGet
         ~@(map getter-impl fields)
         ~'IBodySet
         ~@(mapcat setters-impl fields)))))

(defbody x y z vx vy vz mass)

(defn ^Body jupiter []
  (Body. 4.84143144246472090
         -1.16032004402742839
         -1.03622044471123109e-01
         (* 1.66007664274403694e-03 days-per-year)
         (* 7.69901118419740425e-03 days-per-year)
         (* -6.90460016972063023e-05 days-per-year)
         (* 9.54791938424326609e-04 solar-mass)))

(defn ^Body saturn []
  (Body. 8.34336671824457987
         4.12479856412430479
         -4.03523417114321381e-01
         (* -2.76742510726862411e-03 days-per-year)
         (* 4.99852801234917238e-03 days-per-year)
         (* 2.30417297573763929e-05 days-per-year)
         (* 2.85885980666130812e-04 solar-mass)))

(defn ^Body uranus []
  (Body. 1.28943695621391310e+01
         -1.51111514016986312e+01
         -2.23307578892655734e-01
         (* 2.96460137564761618e-03 days-per-year)
         (* 2.37847173959480950e-03 days-per-year)
         (* -2.96589568540237556e-05 days-per-year)
         (* 4.36624404335156298e-05 solar-mass)))

(defn ^Body neptune []
  (Body. 1.53796971148509165e+01
         -2.59193146099879641e+01
         1.79258772950371181e-01
         (* 2.68067772490389322e-03 days-per-year)
         (* 1.62824170038242295e-03 days-per-year)
         (* -9.51592254519715870e-05 days-per-year)
         (* 5.15138902046611451e-05 solar-mass)))

(defn sun []
  (Body. 0.0 0.0 0.0 0.0 0.0 0.0 solar-mass))

(defn -main [& args]
  (let [bodies (make-array Body 9)
        n (if-let [n (first args)] (Integer/parseInt n) 500000)]
    (format "%.9f\n" ())))