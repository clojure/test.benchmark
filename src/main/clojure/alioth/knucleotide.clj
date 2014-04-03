;   The Computer Language Benchmarks Game
;;   http://benchmarksgame.alioth.debian.org/
;;
;; ported from Scala #2
;; contributed by Alex Miller

(ns alioth.knucleotide
  (:gen-class)
  (:require [clojure.string :as s])
  (:import [java.util.concurrent Executors Future]
           [java.io InputStream]
           [clojure.lang Numbers]))

;;(set! *warn-on-boxed-math* true)
(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def ^:const charmap {\a 0
                      \A 0
                      \t 1
                      \T 1
                      \g 2
                      \G 2
                      \c 3
                      \C 3})

(def ^longs table
  (long-array (for [i (range 256)] (get charmap (char i) -1))))

(definterface IBits
  (add2 [^long b])
  (addLots [bs])
  (scan [^long n ^long offset])
  (^long getSize []))

(deftype Bits [^ints data ;; array of ints, each of which has 16 2-bit slots
               ^:unsynchronized-mutable ^long size ;; number of bits in Bits
               ^:unsynchronized-mutable ^long index ;; write index in data
               ^:unsynchronized-mutable ^long n] ;; bit offset to write at current index
  IBits
  (add2 [_ b]
    (let [data ^ints data]
      (set! size (inc size))
      (when (> n 30)
        (set! index (inc index))
        (set! n 0))
      (aset data index (bit-or (aget ^ints data index)
                               (Numbers/shiftLeftInt (bit-and b 0x3) n)))
      (set! n (+ n 2))
      nil))
  (addLots [this bs]
    (let [data ^ints data
          bs ^Bits bs]
      (if (or (zero? n) (> n 30))
        (do
          (when (> n 30)
            (set! index (inc index))
            (set! n 0))
          (System/arraycopy ^ints (.-data bs) 0 data index (.-index bs))
          (set! index (+ index (.-index bs)))
          (when (and (> index 0) (zero? n))
            (set! index (dec index))
            (set! n 32)))
        (do
          (loop [i 0]
            (when (< i (.-index bs))
              (let [j (aget ^ints (.-data bs) i)]
                (aset data index (bit-or (aget data index) (Numbers/shiftLeftInt j n)))
                (set! index (inc index))
                (aset data index (bit-or (aget data index) (Numbers/unsignedShiftRightInt j (- 32 n))))
                (recur (inc i)))))))
      (set! size (+ size (* 16 ^long (.-index bs))))
      (when (not= (.-n bs) 0)
        (loop [n (.-n bs)
               i (aget ^ints (.-data bs) (.-index bs))]
          (when (> n 0)
            (.add2 this i)
            (recur (long (- n 2))
                   (long (Numbers/unsignedShiftRightInt i 2))))))
      nil))
  (scan [_ n offset]
    (let [data ^ints data
          mask (dec (bit-shift-left 1 (* n 2)))
          scanned (long-array (quot size n))]
      (loop [i (rem offset 16)
             j (quot offset 16)
             scan-index 0]
        (if (<= (+ (* j 16) i n) size)
          (if (<= (+ i n) 16)
            (let [l (bit-and (unsigned-bit-shift-right (aget data j) (* 2 i)) mask)
                  newi (+ i n)]
              (aset scanned scan-index l)
              (if (>= newi 16)
                (recur (- newi 16) (inc j) (inc scan-index))
                (recur newi j (inc scan-index))))
            ;;(bit-and (aget data j) 0xFFFFFFFF)
            (let [l (bit-and (bit-or (Numbers/unsignedShiftRightInt (aget data j) (int (* 2 i)))
                                     (bit-shift-left (aget data (inc j)) (* 2 (- 16 i))))
                             mask)
                  newj (inc j)
                  newi (+ i (- n 16))]
              (aset scanned scan-index l)
              (if (>= newi 16)
                (recur (- newi 16) (inc newj) (inc scan-index))
                (recur newi newj (inc scan-index)))))
          (do
            (loop [pad-index scan-index]
              (if (< pad-index (alength scanned))
                (do (aset scanned scan-index -1)
                    (recur (inc pad-index)))
                scanned)))))))
  (getSize [_] size))

(definterface IDnaHash
  (^alioth.knucleotide.IDnaHash add [^long key ^long count])
  (^long get [^long key])
  (^void printSorted []))

(defn- hc ^long [^long l ^long size]
  (bit-and (+ l (bit-shift-right l 17)) (dec size)))

(defn- nx ^long [^long i ^long size]
  (bit-and (inc i) (dec size)))

(def ^:constant decode ["A" "T" "G" "C"] #_{0 "A" 1 "T" 2 "G" 3 "C"})
(defn l2s [^long l ^long n]
  (if (<= n 0)
    ""
    (str (decode (bit-and l 0x3)) (l2s (unsigned-bit-shift-right l 2) (dec n)))))

(deftype DnaHash [^long z
                  ^long size
                  ^:unsynchronized-mutable ^long n
                  ^longs keys
                  ^longs counters]
  IDnaHash
  (add [this key count]
    (let [size size
          keys ^longs keys
          counters ^longs counters
          index (hc key size)]
      (cond
       ;; new key
       (zero? (aget counters index))
       (do
         (aset keys index key)
         (aset counters index count)
         (set! n (inc n))
         this)

       ;; existing key
       (= (aget keys index) key)
       (do
         (aset counters index (+ (aget counters index) count))
         this)

       ;; rehash
       (> (* 6 n) size)
       (let [newsize (* size 2)
             newhash (DnaHash. z newsize 0 (long-array newsize) (long-array newsize))]
         (loop [i 0]
           (if (< i size)
             (let [ci (aget counters i)]
               (when (> ci 0)
                 (.add newhash (aget keys i) ci))
               (recur (inc i)))
             (do
               (.add newhash key 1)
               newhash))))

       :else-next-location
       (let [i (loop [i (nx index size)]
                 (if (and (not= 0 (aget counters i))
                          (not= key (aget keys i)))
                   (recur (nx i size))
                   i))]
         (if (zero? (aget counters i))
           (do
             (aset keys i key)
             (aset counters i count)
             (set! n (inc n)))
           (aset counters i (+ (aget counters i) count)))
         this))))
  (get [_ key]
    (loop [i (hc key size)]
      (if (and (pos? (aget counters i)) (not= key (aget keys i)))
        (recur i)
        (aget counters i))))
  (printSorted [this]
    (let [tcounts (loop [idx 0 acc 0]
                    (if (< idx (alength counters))
                      (recur (inc idx) (+ acc (aget counters idx)))
                      acc))
          factor (/ 100.0 tcounts)
          freqs (loop [i 0
                       acc (transient [])]
                  (if (< i (alength counters))
                    (let [c (* factor (aget counters i))
                          k (l2s (aget keys i) z)]
                      (recur (inc i) (if (> c 0) (conj! acc [c k]) acc)))
                    (persistent! acc)))
          s (reverse (sort freqs))]
      (doseq [[freq label] s]
        (printf "%s %.3f\n" label freq))
      (println))))

(defn prints [^IDnaHash d ^String s]
  (let [bs (.getBytes s)
        mapped (map #(aget ^longs table (bit-and % 0xFF)) bs)
        k (reduce #(+ (* 4 %1) %2) 0 (reverse mapped))]
    (format "%d\t%s" (.get d k) s)))

(defrecord ReadState [is            ;; input
                      target        ;; target
                      ^long tlen    ;; target
                      ^long need    ;; target
                      data          ;; data read from input
                      ^long n       ;; size of data
                      ^long i       ;; offset in data
                      state         ;; read state: :find :skip :read
                      ])

(defmulti readf (fn [read-state] (:state read-state)))

;; search for target bytes in input
(defmethod readf :find [{:keys (is target tlen need data n i) :as rs}]
  (let [^InputStream is is
        ^bytes data data
        ^bytes target target]
    (loop [i (long i)
           need (long need)]
      (if (< i n)
        (if (< need tlen)
          (if (= (aget data i) (aget target need))
            (recur (inc i) (inc need))
            (recur (inc i) 0))
          (if (= (inc i) n)
            (assoc rs :i 0 :need need :state :skip :n (.read is data))
            (assoc rs :i i :need need :state :skip)))
        (assoc rs :i 0 :need need :n (.read is data))))))

;; skip to end of line with target bytes
(defmethod readf :skip [{:keys (is data n i) :as rs}]
  (let [^InputStream is is
        ^bytes data data]
    (loop [i (long i)]
      (if (< i n)
        (if (= (aget data i) 10) ;; \newline
          (if (= (inc i) n)
            (assoc rs :i 0 :state :read :n (.read is data))
            (assoc rs :i i :state :read))
          (recur (inc i)))))))

(defn r [{:keys (is data n i) :as rs}]
  (let [is ^InputStream is
        data ^bytes data
        table ^longs table]
    (loop [i (long i)
           n (long n)
           bits (->Bits (int-array (inc (Numbers/unsignedShiftRightInt (- n i) 4))) 0 0 0)
           bitsv []]
      (if (< i n)
        (let [b (long (aget data i))]
          (if (= b 10) ;; if newline
            (recur (inc i) n bits bitsv)
            (let [^IBits bits bits]
              (.add2 bits (aget table (bit-and b 0xFF)))
              (recur (inc i) n bits bitsv))))
        (if (< n 0)
          bitsv
          (let [n (.read is data)]
            (recur 0 n (->Bits (int-array (inc (Numbers/unsignedShiftRightInt n 4))) 0 0 0) (conj bitsv bits))))))))

;; actually read the rest of the data
(defmethod readf :read [rs]
  (r rs))

(defn loadf [^String target]
  (let [is System/in
        tb (.getBytes target)
        data (byte-array (* 1024 1024))
        n (.read is data)]
    (loop [read-state (->ReadState System/in tb (alength tb) 0 data n 0 :find)]
      (let [st (readf read-state)]
        (if (instance? ReadState st)
          (recur st)
          (let [total-size (inc (/ (apply + (map #(.getSize ^IBits %) st)) 16))
                all ^IBits (->Bits (int-array total-size) 0 0 0)]
            (doseq [^IBits bits st :when (pos? (.getSize bits))]
              (.addLots all bits))
            all))))))

(defn addToHash [^IBits data ^IDnaHash h ^long n ^long offset]
  (let [s ^longs (.scan data n offset)
        c (alength s)]
    (loop [h h i 0]
      (if (and (< i c))
        (let [si (aget s i)]
          (if (< si 0)
            h
            (recur (.add h si 1) (inc i))))
        h))))

(defn -main [& args]
  (let [sizes [1 2 3 4 6 12 18]
        sequence "GGTATTTTAATTTATAGT"
        data (time (loadf ">THREE"))
        tasks (doall
               (map (fn [n]
                      (fn []
                        (loop [h (DnaHash. n 16 0 (long-array 16) (long-array 16))
                               i 0]
                          (if (< i n)
                            (recur (addToHash data h n i) (inc i))
                            h))))
                    sizes))        
        processors (.. Runtime getRuntime availableProcessors)
        pool (Executors/newFixedThreadPool processors)        
        [f1 f2 :as futures] (.invokeAll pool tasks)]
    (.printSorted ^DnaHash @f1)
    (.printSorted ^DnaHash @f2)

    (loop [[f & fs] (drop 2 futures)
           [s & ss] (drop 2 sizes)]
      (when f
        (println (prints @f (subs sequence 0 s)))
        (recur fs ss)))
    (.shutdown pool)
    (shutdown-agents)))
