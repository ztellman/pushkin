;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns pushkin.position
  (:require
    [clojure.string :as str]))

;;;

(defn position
  "Integer position for a set of x,y coords."
  [dim x y]
  (+ (* y dim) x))

(defn neighbors
  "Neighboring positions for a given integer position."
  [pos dim]
  (let [x (rem pos dim)
        y (int (/ pos dim))]
    (concat
      (when (< 0 x)
        [(position dim (dec x) y)])
      (when (< x (dec dim))
        [(position dim (inc x) y)])
      (when (< 0 y)
        [(position dim x (dec y))])
      (when (< y (dec dim))
        [(position dim x (inc y))]))))

;;;

(defrecord Position
  [value
   color
   liberties
   neighbor-sum
   neighbor-sum-of-squares
   parent
   white-neighbors
   black-neighbors])

(defn initial-position
  [pos dim]
  (let [neighbors (neighbors pos dim)]
    (map->Position
      {:value pos
       :color :empty
       :liberties (count neighbors)
       :neighbor-sum (apply + neighbors)
       :neighbor-sum-of-squares (->> neighbors (map #(* % %)) (apply +))
       :parent pos
       :white-neighbors 0
       :black-neighbors 0})))

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
        (position dim
          (let [val (- (int col) (int \a))]
            (cond
              (< val 8) val
              (= val 8) (throw (Exception. "invalid coordinate"))
              (> val 8) (dec val)))
          (dec (Integer/parseInt (apply str row))))))))
