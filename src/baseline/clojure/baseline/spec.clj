;   Copyright (c) Rich Hickey and contributors.
;   All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns baseline.spec
  (:require alioth.mandelbrot
            alioth.thread-ring))

(def baseline-specs
  [{:id "mandelbrot"
    :target alioth.mandelbrot/run
    :baseline #(alioth.java.mandelbrot/run %)
    :args [16000]
    :tollerances {:runtime 0.25}}

   {:id "thread-ring"
    :target #(alioth.thread-ring/run alioth.thread-ring/AGENT_COUNT %) 
    :baseline #(alioth.java.threadring/run %)
    :args [500000]
    ;:args [50000000]
    :tollerances {:runtime -9.0}}
  ])

(def ^:const DEFAULT-SAMPLE-SIZE 3)
