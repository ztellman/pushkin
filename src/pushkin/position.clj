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
     Array]))

;;;

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

(def neighbors-2x2 (create-neighbor-array 2))
(def neighbors-9x9 (create-neighbor-array 9))
(def neighbors-13x13 (create-neighbor-array 13))
(def neighbors-19x19 (create-neighbor-array 19))

(defn neighbors [^long pos ^long dim]
  (let [ary (case dim
              2 neighbors-2x2
              9 neighbors-9x9
              13 neighbors-13x13
              19 neighbors-19x19)]
    (aget ^"[[J" ary pos)))

(defn ^long num-neighbors [^long pos ^long dim]
  (Array/getLength (neighbors pos dim)))

;;;

(definterface IPosition
  (clone [])
  (color [])
  (set_color [c])
  (^long liberties [])
  (add_liberties [^long n])
  (reset_liberties [])
  (^long neighbor_sum [])
  (add_neighbor_sum [^long n])
  (reset_neighbor_sum [])
  (^long neighbor_sum_of_squares [])
  (add_neighbor_sum_of_squares [^long n])
  (reset_neighbor_sum_of_squares [])
  (^long parent [])
  (set_parent [^long n])
  (^long white_neighbors [])
  (add_white_neighbors [^long n])
  (^long black_neighbors [])
  (add_black_neighbors [^long n]))

(defmacro += [field n]
  `(do
     (set! ~field (long (unchecked-add (long ~field) (long ~n))))
     nil))

(deftype Position
  [^long value
   ^:unsynchronized-mutable color
   ^:unsynchronized-mutable ^long liberties
   ^:unsynchronized-mutable ^long neighbor-sum
   ^:unsynchronized-mutable ^long neighbor-sum-of-squares
   ^:unsynchronized-mutable ^long parent
   ^:unsynchronized-mutable ^long white-neighbors
   ^:unsynchronized-mutable ^long black-neighbors]

  IPosition

  (clone [_]
    (Position.
      value
      color
      liberties
      neighbor-sum
      neighbor-sum-of-squares
      parent
      white-neighbors
      black-neighbors))

  (color [_] color)
  (set_color [_ c] (set! color c) nil)

  (^long liberties [_] liberties)
  (add_liberties [_ ^long n] (+= liberties n))
  (reset_liberties [_] (set! liberties 0) nil)

  (^long neighbor_sum [_] neighbor-sum)
  (add_neighbor_sum [_ ^long n] (+= neighbor-sum n))
  (reset_neighbor_sum [_] (set! neighbor-sum 0) nil)

  (^long neighbor_sum_of_squares [_] neighbor-sum-of-squares)
  (add_neighbor_sum_of_squares [_ ^long n] (+= neighbor-sum-of-squares n))
  (reset_neighbor_sum_of_squares [_] (set! neighbor-sum-of-squares 0) nil)

  (^long parent [_] parent)
  (set_parent [_ ^long n] (set! parent (long n)) nil)

  (^long white_neighbors [_] white-neighbors)
  (add_white_neighbors [_ ^long n] (+= white-neighbors n))

  (^long black_neighbors [_] black-neighbors)
  (add_black_neighbors [_ ^long n] (+= black-neighbors n)))

(defn initial-positions [dim]
  (let [ary (make-array Position (* dim dim))]
    (dotimes [pos (* dim dim)]
      (let [neighbors (neighbors pos dim)]
        (aset ^objects ary pos
          (Position.
            pos
            :empty
            (count neighbors)
            (apply + neighbors)
            (->> neighbors (map #(* % %)) (apply +))
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
  (case color
    :white :black
    :black :white))

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
  (let [pos (str/lower-case pos)]
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
