;;   The Computer Language Benchmarks Game
;;   http://benchmarksgame.alioth.debian.org/

;; adapted from Java #2
;; contributed by Alex Miller

(ns alioth.nbody
  (:gen-class))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; Body = double[x y z vx vy vz mass]
;; System = double[][]

(defn init-system []
  (let [solar-mass (* 3.141592653589793 3.141592653589793 4.0)
        days-per-year 365.24
        sun (double-array [0.0 0.0 0.0 0.0 0.0 0.0 solar-mass])
        jupiter (double-array [4.84143144246472090e+00
                               -1.16032004402742839e+00
                               -1.03622044471123109e-01
                               (* 1.66007664274403694e-03 days-per-year)
                               (* 7.69901118419740425e-03 days-per-year)
                               (* -6.90460016972063023e-05 days-per-year)
                               (* 9.54791938424326609e-04 solar-mass)])
        saturn  (double-array [8.34336671824457987e+00
                               4.12479856412430479e+00
                               -4.03523417114321381e-01
                               (* -2.76742510726862411e-03 days-per-year)
                               (* 4.99852801234917238e-03 days-per-year)
                               (* 2.30417297573763929e-05 days-per-year)
                               (* 2.85885980666130812e-04 solar-mass)])
        uranus (double-array [1.28943695621391310e+01
                              -1.51111514016986312e+01
                              -2.23307578892655734e-01
                              (* 2.96460137564761618e-03 days-per-year)
                              (* 2.37847173959480950e-03 days-per-year)
                              (* -2.96589568540237556e-05 days-per-year)
                              (* 4.36624404335156298e-05 solar-mass)])
        neptune (double-array [1.53796971148509165e+01
                               -2.59193146099879641e+01
                               1.79258772950371181e-01
                               (* 2.68067772490389322e-03 days-per-year)
                               (* 1.62824170038242295e-03 days-per-year)
                               (* -9.51592254519715870e-05 days-per-year)
                               (* 5.15138902046611451e-05 solar-mass)])
        bodies (object-array [sun jupiter saturn uranus neptune])]
    (loop [px 0.0
           py 0.0
           pz 0.0
           i 0]
      (if (< i (alength bodies))
        (let [b ^doubles (aget bodies i)
              bmass (aget b 6)]
          (recur (+ px (* (aget b 3) bmass))
                 (+ py (* (aget b 4) bmass))
                 (+ pz (* (aget b 5) bmass))
                 (inc i)))
        (do
          (aset sun 3 (/ (- 0 px) solar-mass))
          (aset sun 4 (/ (- 0 py) solar-mass))
          (aset sun 5 (/ (- 0 pz) solar-mass))
          bodies)))))

(defn energy ^double [^objects system]
  (let [n (alength system)
        X (int 0)
        Y (int 1)
        Z (int 2)
        VX (int 3)
        VY (int 4)
        VZ (int 5)
        MASS (int 6)]
    (loop [i 0
           j 0
           e 0.0]
      (if (< i n)
        (if (= j 0)
          (let [ib ^doubles (aget system i)
                bvx (aget ib VX)
                bvy (aget ib VY)
                bvz (aget ib VZ)]
            (recur i (inc i) (+ e (* 0.5 (aget ib MASS)
                                     (+ (* bvx bvx)
                                        (* bvy bvy)
                                        (* bvz bvz))))))
          (if (< j n)
            (let [ib ^doubles (aget system i)
                  jb ^doubles (aget system j)
                  dx (- (aget ib X) (aget jb X))
                  dy (- (aget ib Y) (aget jb Y))
                  dz (- (aget ib Z) (aget jb Z))
                  dist (Math/sqrt (+ (* dx dx) (* dy dy) (* dz dz)))]
              (recur i (inc j) (- e (/ (* (aget ib MASS) (aget jb MASS)) dist))))
            (recur (inc i) 0 e)))
        e))))

(defn advance [^objects system ^double dt]
  (let [n (long (alength system))
        X (int 0)
        Y (int 1)
        Z (int 2)
        VX (int 3)
        VY (int 4)
        VZ (int 5)
        MASS (int 6)]
    (loop [i 0
           j 1]
      (if (< i n)
        (if (< j n)
          (let [ib ^doubles (aget system i)
                jb ^doubles (aget system j)
                dx (- (aget ib X) (aget jb X))
                dy (- (aget ib Y) (aget jb Y))
                dz (- (aget ib Z) (aget jb Z))
                dsq (+ (* dx dx) (* dy dy) (* dz dz))
                dist (Math/sqrt dsq)
                mag (/ dt (* dsq dist))
                imassmag (* (aget ib MASS) mag)
                jmassmag (* (aget jb MASS) mag)]
            (aset ib VX (- (aget ib VX) (* dx jmassmag)))
            (aset ib VY (- (aget ib VY) (* dy jmassmag)))
            (aset ib VZ (- (aget ib VZ) (* dz jmassmag)))
            (aset jb VX (+ (aget jb VX) (* dx imassmag)))
            (aset jb VY (+ (aget jb VY) (* dy imassmag)))
            (aset jb VZ (+ (aget jb VZ) (* dz imassmag)))
            (recur i (inc j)))
          (recur (inc i) (+ i 2)))
        (loop [k 0]
          (when (< k n)
            (let [b ^doubles (aget system k)]
              (aset b X (+ (aget b X) (* dt (aget b VX))))
              (aset b Y (+ (aget b Y) (* dt (aget b VY))))
              (aset b Z (+ (aget b Z) (* dt (aget b VZ))))
              (recur (inc k)))))))))

(defn -main [& args]
  (let [n (long (if (pos? (count args)) (Long/parseLong (first args)) 10))
        system ^objects (init-system)]    
    (printf "%.9f\n" (energy system))
    (loop [i 0]
      (if (< i n)
        (do
          (advance system 0.01)
          (recur (inc i)))
        (do
          (printf "%.9f\n" (energy system))
          (flush))))))
