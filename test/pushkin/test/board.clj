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
    [pushkin.core]
    [clojure.test])
  (:require
    [pushkin.position :as p]
    [pushkin.hash :as h]
    [pushkin.board :as b])
  (:import
    [pushkin.position Position]
    [pushkin.board Board]))

;; brute force implementations of board lookups, for validation

(defn neighbors
  ([board pos]
     (neighbors board pos (constantly true)))
  ([board pos predicate]
     (let [ns (atom [])]
       (b/foreach-neighbor board [pos n] []
         (when (predicate (p/color n))
           (swap! ns conj n)))
       @ns)))

(defn group [^Board board n]
  (let [p (b/parent board n)]
    (->> (.positions board)
      (filter #(= p (b/parent board %)))
      set)))

(defn neighbor-sum-of-squares [board pos]
  (->> pos
    (group board)
    (mapcat #(neighbors board % #{:empty}))
    (map #(.value ^Position %))
    (map #(* % %))
    (apply +)))

(defn neighbor-sum [board pos]
  (->> pos
    (group board)
    (mapcat #(neighbors board % #{:empty}))
    (map #(.value ^Position %))
    (apply +)))

(defn liberties [board pos]
  (->> pos
    (group board)
    (mapcat #(neighbors board % #{:empty}))
    count))

(defn black-neighbors [board pos]
  (->> pos
    (group board)
    (mapcat #(neighbors board % #{:black}))
    count))

(defn white-neighbors [board pos]
  (->> pos
    (group board)
    (mapcat #(neighbors board % #{:white}))
    count))

(defn atari [board pos]
  (let [neighbors (->> pos
                    (group board)
                    (mapcat #(neighbors board % #{:empty}))
                    set)]
    (when (= 1 (count neighbors))
      (.value ^Position (first neighbors)))))

(defn capture [board color p]
  (->> (neighbors board p #{(p/opponent color)})
    (filter #(atari board %))
    first))

(defn eye? [board p]
  (let [num-neighbors (count (neighbors board p))]
    (when-let [v (or
                   (and
                     (= num-neighbors (white-neighbors board p))
                     (not (capture board :black p))
                     :white)
                   (and
                     (= num-neighbors (black-neighbors board p))
                     (not (capture board :white p))
                     :black))]
      v)))

(defn suicide? [board color p]
  (let [num-neighbors (count (neighbors board p))
        same-neighbors (neighbors board p #{color})
        diff-neighbors (neighbors board p #{(p/opponent color)})]
    (and
      (= num-neighbors (+ (count same-neighbors) (count diff-neighbors)))
      (not (some #(atari board %) diff-neighbors))
      (every? #(atari board %) same-neighbors))))

(def parent-field-validations
  [[:sum (constantly true) p/sum neighbor-sum]
   [:sum-of-squares (constantly true) p/sum-of-squares neighbor-sum-of-squares]
   [:liberties (constantly true) p/liberties liberties]
   [:white-neighbors #{:empty} p/white-neighbors white-neighbors]
   [:black-neighbors #{:empty} p/black-neighbors black-neighbors]
   [:atari #{:white :black} p/atari atari]])

(def board-validations
  [[:eye #{:empty} b/eye? eye?]])

(def move-validations
  [[:capture b/capture capture]
   [:suicide b/suicide? suicide?]])

(defn validate-board [^Board board]
  (try
    (doseq [^Position p (.positions board)]

      (let [coord (p/position->gtp (.value p) (b/dim board))]

        ;; parent field validations
        (doseq [[field predicate lookup from-scratch] parent-field-validations]
          (when (predicate (p/color p))
            (let [expected (from-scratch board p)
                  actual (lookup (b/parent board p))]
              (is (= expected actual) (str field " at " coord)))))

        ;; board validations
        (doseq [[field predicate lookup from-scratch] board-validations]
          (when (predicate (p/color p))
            (let [expected (from-scratch board p)
                  actual (lookup board p)]
              (is (= expected actual) (str field " at " coord)))))

        ;; move validations
        (doseq [[field lookup from-scratch] move-validations]
          (when (= :empty (p/color p))
            (doseq [color [:black :white]]
              (let [expected (from-scratch board color p)
                    actual (lookup board color p)]
                (is (= expected actual) (str field " at " coord ", " color))))))))

    (catch Exception e
      (b/print-board board)
      (throw e))))

;;;

(defn board [dim & color-move-pairs]
  (let [pairs (partition 2 color-move-pairs)]
    (let [board (b/empty-board dim)]
      (doseq [[color move] pairs]
        (let [color (case color
                      :b :black
                      :w :white
                      color)
              move (p/gtp->position move dim)]
          (b/add-stone board (b/position board move) color)))
      board)))

;;  ABCDEFGHJ
;;
;; 4 ......... 4
;; 3 ......OOO 3
;; 2 ......OXX 2
;; 1 ......OX. 1
;;
;;   ABCDEFGHJ

(deftest test-board-position
  (let [board (board 9
                :b "J2", :b "H2", :b "H1"
                :w "J3", :w "H3", :w "G3", :w "G2", :w "G1")
        black-parent (b/parent board (b/position board 7))
        white-parent (b/parent board (b/position board 6))
        capture-point (b/position board 8)]
    (is (= 2 (p/liberties black-parent)))
    (is (= 16 (p/sum black-parent)))
    (is (= 128 (p/sum-of-squares black-parent)))
    (is (= 8 (p/atari black-parent)))
    (is (= nil (p/atari white-parent)))
    (is (b/capture board :white capture-point))
    (is (b/capture-all? board :white capture-point))
    (is (b/suicide? board :black capture-point))
    (is (not (b/suicide? board :white capture-point)))))

;;;

(deftest ^:benchmark benchmark-tests
  (let [board (board 9
                :b "J2", :b "H2", :b "H1"
                :w "J3", :w "H3", :w "G3", :w "G2", :w "G1")
        black-parent (b/position board 7)
        black-child (b/position board 18)
        white-parent (b/position board 6)
        capture-point (b/position board 8)]
    (bench "parent"
      (b/parent board black-child))
    (bench "atari"
      (p/atari black-parent))
    (bench "eye?"
      (b/eye? board capture-point))
    (bench "capture"
      (b/capture board :white capture-point))
    (bench "suicide?"
      (b/suicide? board :black capture-point))))

#_(deftest ^:benchmark benchmark-add-remove-stone
  (let [board (b/empty-board 9)
        pos (b/position board 42)]
    (let [board (clone board)]
      (long-bench "add stone"
        (b/add-stone board pos :black)))
    (long-bench "add stone, then remove"
      (b/add-stone board pos :black)
      (b/remove-stone board pos))))

