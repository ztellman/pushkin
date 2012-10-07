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

(defn group [board n]
  (let [p (b/parent board n)]
    (->> board
      b/position-range
      (filter #(= p (b/parent board %)))
      set)))

(defn neighbor-sum-of-squares [board pos]
  (->> pos
    (group board)
    (mapcat #(b/neighbors board % #{:empty}))
    (map #(* % %))
    (apply +)))

(defn neighbor-sum [board pos]
  (->> pos
    (group board)
    (mapcat #(b/neighbors board % #{:empty}))
    (apply +)))

(defn liberties [board pos]
  (->> pos
    (b/group board)
    (mapcat #(b/neighbors board % #{:empty}))
    count))

(defn black-neighbors [board pos]
  (->> (b/neighbors board pos #{:black})
    count))

(defn white-neighbors [board pos]
  (->> (b/neighbors board pos #{:white})
    count))

(defn atari? [board pos]
  (->> pos
    (group board)
    (mapcat #(b/neighbors board % #{:empty}))
    set
    count
    (= 1)))

(defn capture? [board color p]
  (->> (b/neighbors board p #{(p/opponent color)})
    (filter #(atari? board %))
    first))

(defn eye? [board p]
  (let [num-neighbors (count (p/neighbors p (:dim board)))]
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
      (-> board
        :hash
        h/rotate-hashes
        (h/update-hash p color)
        (h/update-hash n (p/opponent color))
        h/cycle?))))

(defn suicide? [board color p]
  (let [num-neighbors (count (p/neighbors p (:dim board)))
        same-neighbors (b/neighbors board p #{color})
        diff-neighbors (b/neighbors board p #{(p/opponent color)})]
    (and
      (= num-neighbors (+ (count same-neighbors) (count diff-neighbors)))
      (not (some #(atari? board %) diff-neighbors))
      (every? #(atari? board %) same-neighbors))))

(def board-validations
  [[:white-neighbors (constantly true) b/white-neighbors white-neighbors]
   [:black-neighbors (constantly true) b/black-neighbors black-neighbors]
   [:sum #{:white :black} b/neighbor-sum neighbor-sum]
   [:sum-of-squares #{:white :black} b/neighbor-sum-of-squares neighbor-sum-of-squares]
   [:group #{:white :black} b/group group]
   [:liberties #{:white :black} b/liberties liberties]
   [:atari #{:white :black} b/atari? atari?]
   [:eye #{:white :black} b/eye? eye?]])

(def move-validations
  [[:capture b/capture? capture?]
   [:suicide b/suicide? suicide?]
   [:ko b/ko? ko?]])

(defn validate-board [board]
  (try
    (doseq [p (b/position-range board)]

      (let [coord (p/position->gtp p (:dim board))]

        ;; board validations
        (doseq [[field predicate lookup from-scratch] board-validations]
          (when (predicate (b/color board p))
            (let [expected (from-scratch board p)
                  actual (lookup board p)]
              (is
                (= expected actual)
                (str field " at " coord ": " actual ", " expected)))))

        ;; move validations
        (doseq [[field lookup from-scratch] move-validations]
          (doseq [color [:black :white]]
            (let [expected (from-scratch board color p)
                  actual (lookup board color p)]
              (is
                (= expected actual)
                (str field " at " coord ": " actual ", " expected)))))))

    (catch Exception e
      (b/print-board board)
      (throw e))))

;;;

(deftest ^:benchmark benchmark-add-remove-stone
  (let [board (b/empty-board 9)]
    (bench "add stone, then remove"
      (let [pos (rand-int 81)]
        (-> board
          (b/add-stone pos :black)
          (b/remove-stone pos))))))
