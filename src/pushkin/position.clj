;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns pushkin.position
  (:use
    [potemkin :only (unify-gensyms)]
    [clojure walk]
    [useful.datatypes])
  (:require
    [clojure.string :as str])
  (:import
    [java.lang.reflect
     Array])
  (:refer-clojure
    :exclude
    [pop! contains?]))

;;;

(set! *unchecked-math* true)

(defn coord->position
  "Integer position for a set of x,y coords."
  [dim x y]
  (+ (* y dim) x))

(defn calculate-neighbors [pos dim]
  (let [x (rem pos dim)
        y (int (/ pos dim))]
    (concat
      (when (< 0 x)
        [(coord->position dim (dec x) y)])
      (when (< x (dec dim))
        [(coord->position dim (inc x) y)])
      (when (< 0 y)
        [(coord->position dim x (dec y))])
      (when (< y (dec dim))
        [(coord->position dim x (inc y))]))))

(defn create-neighbor-array [dim]
  (let [ary (make-array (Class/forName "[J") (* dim dim))]
    (dotimes [i (* dim dim)]
      (let [neighbors (into-array Long/TYPE (calculate-neighbors i dim))]
        (aset ^objects ary i neighbors)))
    ary))

(let [neighbors-2x2 (create-neighbor-array 2)
      neighbors-9x9 (create-neighbor-array 9)
      neighbors-13x13 (create-neighbor-array 13)
      neighbors-19x19 (create-neighbor-array 19)]

  (defn neighbors [^long pos ^long dim]
    (let [ary (case dim
                2 neighbors-2x2
                9 neighbors-9x9
                13 neighbors-13x13
                19 neighbors-19x19)]
      (aget ^"[[J" ary pos))))

(defn ^long num-neighbors [^long pos ^long dim]
  (Array/getLength (neighbors pos dim)))

;;;

(defprotocol IPosition
  (position->map [_])
  (clone [_])
  (color [_])
  (set-color [_ c])
  (reset [_])
  (surrounded? [_])
  (add-neighbor [_ p])
  (remove-neighbor [_ p])
  (unify [_ p])
  (set-parent [_ p])
  (liberties [_])
  (sum [_])
  (sum-of-squares [_])
  (white-neighbors [_])
  (black-neighbors [_])
  (parent [_])
  (atari [_]))

(defmacro += [field n]
  `(do
     (set! ~field (long (+ (long ~field) (long ~n))))
     nil))

(defmacro -= [field n]
  `(do
     (set! ~field (long (- (long ~field) (long ~n))))
     nil))

(deftype Position
  [^long value
   ^long initial-liberties
   ^long initial-sum
   ^long initial-sum-of-squares
   ^:unsynchronized-mutable -color
   ^:unsynchronized-mutable ^long -liberties
   ^:unsynchronized-mutable ^long -sum
   ^:unsynchronized-mutable ^long -sum-of-squares
   ^:unsynchronized-mutable ^long -parent
   ^:unsynchronized-mutable ^long -white-neighbors
   ^:unsynchronized-mutable ^long -black-neighbors]

  IPosition

  (position->map [_]
    {:value value
     :color -color
     :liberties -liberties
     :sum -sum
     :sum-of-squares -sum-of-squares
     :white-neighbors -white-neighbors
     :black-neighbors -black-neighbors})

  (clone [_]
    (Position.
      value
      initial-liberties
      initial-sum
      initial-sum-of-squares
      -color
      -liberties
      -sum
      -sum-of-squares
      -parent
      -white-neighbors
      -black-neighbors))

  (color [_] -color)
  (set-color [_ c] (set! -color c) nil)
  (parent [_] -parent)
  (set-parent [_ p] (set! -parent (.value ^Position p)) nil)
  (liberties [_] -liberties)
  (sum [_] -sum)
  (sum-of-squares [_] -sum-of-squares)
  (white-neighbors [_] -white-neighbors)
  (black-neighbors [_] -black-neighbors)

  (reset [_]
    (set! -color :empty)
    (set! -parent value)
    (set! -liberties initial-liberties)
    (set! -sum initial-sum)
    (set! -sum-of-squares initial-sum-of-squares)
    (set! -white-neighbors 0)
    (set! -black-neighbors 0)
    nil)

  (surrounded? [_]
    (if (== initial-liberties -white-neighbors)
      :white
      (if (== initial-liberties -black-neighbors)
        :black
        (if (== initial-liberties (+ -white-neighbors -black-neighbors))
          :mixed
          nil))))

  (add-neighbor [_ p]
    (let [^Position p p
          val (.value p)]
      (-= -liberties 1)
      (-= -sum (long val))
      (-= -sum-of-squares (* (long val) (long val)))
      (if (identical? :white (color p))
        (+= -white-neighbors 1)
        (+= -black-neighbors 1))
      nil))

  (remove-neighbor [_ p]
    (let [^Position p p
          val (.value p)]
      (+= -liberties 1)
      (+= -sum (long val))
      (+= -sum-of-squares (* (long val) (long val)))
      (if (identical? :white (color p))
        (-= -white-neighbors 1)
        (-= -black-neighbors 1))
      nil))

  (unify [this p]
    (let [^Position p p]
      (when (not (== (long value) (long (parent p))))
        (set-parent p this)
        (+= -liberties (liberties p))
        (+= -sum (sum p))
        (+= -sum-of-squares (sum-of-squares p))))
    nil)

  (atari [_]
    (when (and
            (<= (long -liberties) 4)
            (==
              (* (long -sum) (long -sum))
              (* (long -liberties) (long -sum-of-squares))))
      (unchecked-divide-int (int -sum) (int -liberties)))))

(defn initial-positions [dim]
  (let [ary (make-array Position (* dim dim))]
    (dotimes [pos (* dim dim)]
      (let [neighbors (neighbors pos dim)
            liberties (count neighbors)
            sum (apply + neighbors)
            sum-of-squares (->> neighbors (map #(* % %)) (apply +))]
        (aset ^objects ary pos
          (Position.
            pos
            liberties
            sum
            sum-of-squares
            :empty
            liberties
            sum
            sum-of-squares
            pos
            0
            0))))
    ary))

(defmacro position [ary n]
  (with-meta
    `(aget ~(with-meta ary {:tag "objects"}) ~n)
    {:tag "pushkin.position.Position"}))

(defn clone-positions [^objects ary]
  (let [cnt (Array/getLength ary)
        ^objects copy (make-array Position cnt)]
    (dotimes [i cnt]
      (let [^Position p (aget ary i)]
        (aset copy i (.clone p))))
    copy))

;;;

(defn- unrolled-action [neighbor body val index]
  `(~neighbor (pushkin.position/position positions## (aget neighbors## ~index))
    ~val (do ~@body)))

;; just a tiny bit awful
(defmacro foreach-neighbor [dim positions [pos neighbor] [val initial-value] & body]
  (let [val (or val '_)]
    (unify-gensyms
      `(let [^objects positions## ~positions
             ^"[J" neighbors## (neighbors ~pos ~dim)
             ~val ~initial-value]
         (case (long (Array/getLength neighbors##))
           
           2 (let [~@(mapcat #(unrolled-action neighbor body val %) (range 2))]
               ~val)
           
           3 (let [~@(mapcat #(unrolled-action neighbor body val %) (range 3))]
               ~val)
           
           4 (let [~@(mapcat #(unrolled-action neighbor body val %) (range 4))]
               ~val))))))

;;;

(defn opponent [color]
  (if (identical? :black color)
    :white
    :black))

;;;

(defn position->gtp [pos dim]
  (if (= :pass pos)
    "PASS"
    (let [x (rem pos dim)
          x (if (<= 8 x)
              (inc x)
              x)
          y (int (/ pos dim))]
      (str
        (char (+ (int \A) x))
        (inc y)))))

(defn gtp->position [pos dim]
  (let [pos (str/lower-case (name pos))]
    (if (= "pass" pos)
      :pass
      (let [[col & row] pos]
        (coord->position dim
          (let [val (- (int col) (int \a))]
            (cond
              (< val 8) val
              (= val 8) (throw (Exception. "invalid coordinate"))
              (> val 8) (dec val)))
          (dec (Integer/parseInt (apply str row))))))))
