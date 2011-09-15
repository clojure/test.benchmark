;   Copyright (c) Rich Hickey and contributors.
;   All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns test.alioth.mandelbrot
    (:use [alioth.mandelbrot]
          [alioth.util.core]
          [clojure.java.io :only [input-stream]]
          [clojure.test])
    (:import [java.io ByteArrayOutputStream
                      ByteArrayInputStream]))

(def ^:private master-files {8   "m8.bmp",
                             256 "m256.bmp"})

(def ^:private this-ns *ns*) ; work around bug in com.theoryinpractice.clojure.testrunner 
                             ; leiningen has a similar bug, namely,
                             ; the runner hijacks the current namepsace.

(defn- resource-stream [file-name]
  (input-stream (ns-resource this-ns file-name)))

(defn- matches-master [dimension mandelbrot-fn write-fn]
  (with-open [outStream (write-fn (ByteArrayOutputStream.)
                                  (mandelbrot-fn dimension))
              inStream (ByteArrayInputStream. (.toByteArray outStream))
              masterInStream (resource-stream (master-files dimension))]
    (streams= inStream masterInStream)))


(deftest test-mandelbrot-correctness
  (are [dimension mandelbrot-fn write-fn] (matches-master dimension mandelbrot-fn write-fn)
       8 compute-mandelbrot write-bmp
       256 compute-mandelbrot write-bmp
       ))
