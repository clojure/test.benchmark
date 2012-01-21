;   Copyright (c) Rich Hickey and contributors.
;   All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;   Alioth benchmarks: http://shootout.alioth.debian.org/u64q/benchmark.php?test=spectralnorm&lang=all
;   Inspired by http://shootout.alioth.debian.org/u64q/program.php?test=spectralnorm&lang=java&id=1

(ns alioth.spectral-norm
  (:import [java.text DecimalFormat NumberFormat])
  (:gen-class))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defmacro for-loop [[binding pred adv] & body]
  `(loop [~@binding]
     (when ~pred
       ~@body
       (recur ~adv))))

(defn a ^double [^long i ^long j]
  (/ 1.0 (+ (/ (* (+ i j) (+ i j 1)) 2.0) i 1)))

(defn multiply-av [^long n ^doubles v ^doubles av]
  (for-loop [(i 0) (< i n) (inc i)]
    (aset av i 0.0)
    (for-loop [(j 0) (< j n) (inc j)]
      (aset av i (+ (aget av i) (* (a i j) (aget v j)))))))

(defn multiply-atv [^long n ^doubles v ^doubles atv]
  (for-loop [(i 0) (< i n) (inc i)]
    (aset atv i 0.0)
    (for-loop [(j 0) (< j n) (inc j)]
      (aset atv i (+ (aget atv i) (* (a j i) (aget v j)))))))

(defn multiply-atav [^long n ^doubles v ^doubles atav]
  (let [u (double-array n)]
    (multiply-av n v u)
    (multiply-atv n u atav)))

(defn approximate ^double [^long n]
  (let [u (double-array n)
        v (double-array n)]
    (for-loop [(i 0) (< i n) (inc i)] (aset u i 1.0))
    (for-loop [(i 0) (< i n) (inc i)] (aset v i 0.0))
    (for-loop [(i 0) (< i 10) (inc i)]
      (multiply-atav n u v)
      (multiply-atav n v u))
    (loop [i 0 vbv 0.0 vv 0.0]
      (if (< i n)
        (recur (inc i)
               (+ vbv (* (aget u i) (aget v i)))
               (+ vv (* (aget v i) (aget v i))))
        (Math/sqrt (/ vbv vv))))))

(defn -main [& args]
  (let [formatter (DecimalFormat. "#.000000000")
        n (if-let [n (first args)] (Integer/parseInt n) 100)]
    (println (.format formatter (approximate (long n))))))