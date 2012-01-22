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
  (:import [java.text DecimalFormat])
  (:gen-class))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defn a ^double [^long i ^long j]
  (/ 1.0 (+ (/ (* (+ i j) (+ i j 1)) 2.0) i 1)))

(defn mul-av [^long n ^doubles v ^doubles av]
  (dotimes [i n]
    (aset av i 0.0)
    (dotimes [j n]
      (aset av i (+ (aget av i) (* (a i j) (aget v j)))))))

(defn mul-atv [^long n ^doubles v ^doubles atv]
  (dotimes [i n]
    (aset atv i 0.0)
    (dotimes [j n]
      (aset atv i (+ (aget atv i) (* (a j i) (aget v j)))))))

(defn mul-atav [^long n ^doubles v ^doubles atav]
  (let [u (double-array n)]
    (mul-av n v u)
    (mul-atv n u atav)))

(defn approximate ^double [^long n]
  (let [u (double-array n)
        v (double-array n)]
    (dotimes [i n] (aset u i 1.0) (aset v i 0.0))
    (dotimes [i 10]
      (mul-atav n u v)
      (mul-atav n v u))
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