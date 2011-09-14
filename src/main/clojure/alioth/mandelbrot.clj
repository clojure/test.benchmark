;   Copyright (c) Rich Hickey and contributors.
;   All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.
;
;   Alioth benchmarks: http://shootout.alioth.debian.org/u64q/benchmark.php?test=mandelbrot
;   Partly inspired by: http://shootout.alioth.debian.org/u32/benchmark.php?test=mandelbrot&lang=java
(ns alioth.mandelbrot 
  (:import [java.io OutputStream BufferedOutputStream])
  (:gen-class))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def ^:const MAX_ITERATIONS 49)

(defmacro for-loop [[binding pred adv] & body]
  `(loop [~@binding]
     (when ~pred
       ~@body
       (recur ~adv))))

(definterface MandelbrotOps
  (generate [])
  (putLine [^long y])
  (^byte getByte [^long x ^long y])
  (^long getByte_inner [^long x ^long y ^long i]))

(deftype MandelbrotJob [^bytes out ^long n ^long m ^doubles crb ^doubles cib]
  MandelbrotOps

  (generate [this]
    (let [yIdx (atom -1)
          runner (reify Runnable
                   (run [_] (let [y (swap! yIdx inc)]
                              (if (< y n) (do (.putLine this y) (recur))))))
          pool (map (fn [_] (Thread. runner)) (range (* 2 (.availableProcessors (Runtime/getRuntime)))))]
      (doseq [^Thread thread pool]
        (.start thread)) 
      (doseq [^Thread thread pool]
        (.join thread))))

  (putLine [this y]
    (let [offset (* y m)]
      (for-loop [(xb 0) (< xb m) (inc xb)]
        (aset out (+ offset xb) (.getByte this (* 8 xb) y)))))

  (getByte [this x y]
    (loop [i 0 res 0]
      (if (< i 8)
        (recur (+ i 2) (+ (bit-shift-left res 2)
                          (.getByte_inner this x y i)))
        (byte (bit-xor res -1)))))
  
  (getByte_inner [_ x y i]
    (let [ciby     (aget cib y)
          crbx+i   (aget crb (+ x i))
          crbx+i+1 (aget crb (+ x i 1))]
      (loop [j 0
             zr1 crbx+i   zi1 ciby
             zr2 crbx+i+1 zi2 ciby 
             b 0]
            (if (< j MAX_ITERATIONS)
              (let [nzr1 (+ crbx+i   (- (* zr1 zr1) (* zi1 zi1))) 
                    nzi1 (+ ciby (* zr1 zi1 2))
                    nzr2 (+ crbx+i+1 (- (* zr2 zr2) (* zi2 zi2)))
                    nzi2 (+ ciby (* zr2 zi2 2))
                    nb (if (> (+ (* nzr1 nzr1) (* nzi1 nzi1)) 4) (bit-or b 2) b)
                    nb (if (> (+ (* nzr2 nzr2) (* nzi2 nzi2)) 4) (bit-or nb 1) nb)]
                (if (= nb 3)
                  nb
                  (recur (inc j) nzr1 nzi1 nzr2 nzi2 nb)))
              b)))))

(defn ^MandelbrotJob compute-mandelbrot [^long n]
  (let [n+7 (+ n 7)
        m (/ n+7 8)
        ^bytes out (byte-array (* n m))
        ^doubles crb (double-array n+7)
        ^doubles cib (double-array n+7)
        invN (/ 2.0 n)]
    (for-loop [(i 0) (< i n) (inc i)]
      (aset crb i (- (* i invN) 1.5))
      (aset cib i (- (* i invN) 1.0)))
    (let [^MandelbrotJob job (MandelbrotJob. out n m crb cib)]
      (.generate job) job)))

(defn write-bmp [^MandelbrotJob job ^OutputStream outStream]
  (let [^bytes out (.out job)
        len (* (.m job) (.n job))] ; (.length out) <-- won't compile ??
    (.write outStream (.getBytes (str "P4\n" (.n job) " " (.n job) "\n"))) 
    (.write outStream out 0 len) ; puzzling bug, just (.write outStream out) prints garbage
    outStream))

(defn -main [& args]
  (let [n (if (first args) (Integer/parseInt (first args)) 16000)]
    (with-open [outStream (BufferedOutputStream. System/out)]
      (write-bmp (compute-mandelbrot n) outStream))))
