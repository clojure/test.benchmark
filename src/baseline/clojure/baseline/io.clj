;   Copyright (c) Rich Hickey and contributors.
;   All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns baseline.io
  (:import [java.io FileOutputStream
                    BufferedOutputStream
                    OutputStreamWriter
                    PrintStream
                    ]))

(defmacro with-null-out [& body]
  `(let [original-printer# System/out]
     (with-open [dev-null-stream# (-> "/dev/null"
                                      FileOutputStream.
                                      BufferedOutputStream.)
                dev-null-writer# (OutputStreamWriter. dev-null-stream#)
                dev-null-printer# (PrintStream. dev-null-stream#)]
      (binding [*out* dev-null-writer#]
        (System/setOut dev-null-printer#)
        (let [ret# (do ~@body)] 
          (System/setOut original-printer#)
          ret#)))))
