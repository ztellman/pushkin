;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns pushkin.test.board
  (:use
    [pushkin.test.core]
    [clojure.test])
  (:require
    [pushkin.position :as p]
    [pushkin.hash :as h]
    [pushkin.board :as b]))

;; brute force implementations of board lookups, for validation

(def neighbors b/neighbors)

(defn group [board n]
  (let [p (b/parent board n)]
    (->> (.positions board)
      (filter #(= p (b/parent board %)))
      set)))

(defn neighbor-sum-of-squares [board pos]
  (->> pos
    (group board)
    (mapcat #(neighbors board % #{:empty}))
    (map #(.value %))
    (map #(* % %))
    (apply +)))

(defn neighbor-sum [board pos]
  (->> pos
    (group board)
    (mapcat #(neighbors board % #{:empty}))
    (map #(.value %))
    (apply +)))

(defn liberties [board pos]
  (->> pos
    (group board)
    (mapcat #(neighbors board % #{:empty}))
    count))

(defn black-neighbors [board pos]
  (->> (neighbors board pos #{:black})
    count))

(defn white-neighbors [board pos]
  (->> (neighbors board pos #{:white})
    count))

(defn atari? [board pos]
  (->> pos
    (group board)
    (mapcat #(neighbors board % #{:empty}))
    set
    count
    (= 1)))

(defn capture? [board color p]
  (->> (neighbors board p #{(p/opponent color)})
    (filter #(atari? board %))
    first))

(defn eye? [board p]
  (let [num-neighbors (count (neighbors board p))]
    (or
      (and
        (= num-neighbors (white-neighbors board p))
        (not (capture? board :black p))
        :white)
      (and
        (= num-neighbors (black-neighbors board p))
        (not (capture? board :white p))
        :black))))

(defn ko? [board color p]
  (when-let [n (capture? board color p)]
    (and
      (= n (b/parent board n))
      (case color
        :white (h/ko? (.hash board) p n)
        :black (h/ko? (.hash board) n p)))))

(defn suicide? [board color p]
  (let [num-neighbors (count (neighbors board p))
        same-neighbors (neighbors board p #{color})
        diff-neighbors (neighbors board p #{(p/opponent color)})]
    (and
      (= num-neighbors (+ (count same-neighbors) (count diff-neighbors)))
      (not (some #(atari? board %) diff-neighbors))
      (every? #(atari? board %) same-neighbors))))

(def field-validations
  [[:white-neighbors (constantly true) b/white-neighbors white-neighbors]
   [:black-neighbors (constantly true) b/black-neighbors black-neighbors]])

(def parent-field-validations
  [[:sum (constantly true) b/neighbor-sum neighbor-sum]
   [:sum-of-squares (constantly true) b/neighbor-sum-of-squares neighbor-sum-of-squares]
   [:liberties (constantly true) b/liberties liberties]])

(def board-validations
  [[:atari #{:white :black} b/atari? atari?]
   [:eye #{:white :black} b/eye? eye?]])

(def move-validations
  [[:capture b/capture? capture?]
   [:suicide b/suicide? suicide?]
   [:ko b/ko? ko?]])

(defn validate-board [board]
  (try
    (doseq [p (.positions board)]

      (let [coord (p/position->gtp (.value p) (b/dim board))]

        ;; field validations
        (doseq [[field predicate lookup from-scratch] field-validations]
          (when (predicate (b/color p))
            (let [expected (from-scratch board p)
                  actual (lookup p)]
              (is (= expected actual) (str field " at " coord)))))

        ;; parent field validations
        (doseq [[field predicate lookup from-scratch] parent-field-validations]
          (when (predicate (b/color p))
            (let [expected (from-scratch board p)
                  actual (lookup (b/parent board p))]
              (is (= expected actual) (str field " at " coord)))))

        ;; board validations
        (doseq [[field predicate lookup from-scratch] board-validations]
          (when (predicate (b/color p))
            (let [expected (from-scratch board p)
                  actual (lookup board p)]
              (is (= expected actual) (str field " at " coord)))))

        ;; move validations
        (doseq [[field lookup from-scratch] move-validations]
          (when (= :empty (b/color p))
            (doseq [color [:black :white]]
              (let [expected (from-scratch board color p)
                    actual (lookup board color p)]
                (is (= expected actual) (str field " at " coord))))))))

    (catch Exception e
      (b/print-board board)
      (throw e))))

;;;

(deftest ^:benchmark benchmark-accessors
  (let [board (b/empty-board 9)
        pos (b/position board 42)]
    (long-bench "get color"
      (b/color pos))
    (long-bench "set color"
      (b/set-color pos :white))
    (long-bench "get liberties"
      (b/liberties pos))
    (long-bench "add liberties"
      (b/add-liberties pos 1))
    (long-bench "reset liberties"
      (b/reset-liberties pos))))

(deftest ^:benchmark benchmark-add-remove-stone
  (let [board (b/empty-board 9)
        pos (b/position board 42)]
    (long-bench "add stone, then remove"
      (b/add-stone board pos :black)
      (b/remove-stone board pos))))

