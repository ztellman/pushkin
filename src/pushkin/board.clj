;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns pushkin.board
  (:require
    [pushkin.position :as p]))

;;;

(defrecord Board
  [dim
   eyes
   empty-positions
   positions
   white-score
   black-score])

(defmacro defaccessor [name field]
  `(defn ~name [board# pos#]
     (get-in board# [:positions pos# ~field])))

(def empty-board
  (memoize
    (fn [dim]
      (let [ps (range (* dim dim))]
        (map->Board
          {:dim dim
           :eyes #{}
           :empty-positions (set ps)
           :positions (vec (map #(p/initial-position dim %) ps))
           :white-score 0
           :black-score 0})))))

;;;

(defn print-board [board]
  (let [dim (:dim board)
        cols (->> (range dim)
               (map #(char (+ (int \A) %)))
               (apply str))]
    (println (str " " cols))
    (dotimes [x dim]
      (print (inc x))
      (dotimes [y dim]
        (let [c (get-in board [:positions (p/position dim x y) :color])]
          (print
            (case c
              :white "O"
              :black "X"
              :empty " "))))
      (println (inc x)))
    (println (str " " cols))))

;;;

(defaccessor parent :parent)

(defn ultimate-parent [board pos]
  (loop [pos pos]
    (let [p (parent board pos)]
      (if (= p pos)
        p
        (recur p)))))

(defn penultimate-parent [board pos]
  (let [n (parent board pos)
        nn (parent board n)]
    (loop [a pos, b n, c nn]
     (if (= b c)
       a
       (recur b c (parent c))))))

(defn update-parent [board pos parent]
  (assoc-in board [:positions pos :parent] parent))

;;;

(defaccessor color :color)

(defn update-color [board pos color]
  (assoc-in board [:positions pos :color] color))

(defn neighbors [board pos predicate]
  (->> pos
    p/neighbors
    (filter #(predicate (color board %)))))

;;;

(defaccessor liberties :liberties)

(defn calculate-liberties [board pos]
  (->> (neighbors board pos #{:empty})
    count))

;;;

(defaccessor neighbor-sum :neighbor-sum)

(defn calculate-neighbor-sum [board pos]
  (->> (neighbors board pos #{:empty})
    (apply +)))

;;;

(defaccessor neighbor-sum-of-squares :neighbor-sum-of-squares)

(defn calculate-neighbor-sum-of-squares [board pos]
  (->> (neighbors board pos #{:empty})
    (map #(* % %))
    (apply +)))

;;;

(defaccessor white-neighbors :white-neighbors)

(defn calculate-white-neighbors [board pos]
  (->> (neighbors board pos #{:white})
    count))

;;;

(defaccessor black-neighbors :black-neighbors)

(defn calculate-white-neighbors [board pos]
  (->> (neighbors board pos #{:black})
    count))

;;;

(defn add-stone [board pos stone-color]
  (let [board (-> board
                (assoc-in [:positions pos :color] stone-color)
                (update-in [:empty-positions] disj pos))
        neighbor-count (case stone-color
                         :white :white-neighbors
                         :black :black-neighbors)]
    (reduce
      (fn [board n]
        (let [board (-> board
                      (update-in [:positions n :liberties] dec)
                      (update-in [:positions n :neighbor-sum] #(- % pos))
                      (update-in [:positions n :neighbor-sum-of-squares] #(- % (* pos pos)))
                      (update-in [:positions n neighbor-count] inc))
              n-color (color board n)]
          (if (= stone-color n-color)
            (update-parent board (ultimate-parent board n) pos)
            board)))
      board
      (p/neighbors (:dim board) pos))))

(defn remove-stone
  [board pos]
  (let [pos-color (color board pos)
        board (-> board
                (assoc-in [:positions pos :color] :empty)
                (update-in [:empty-positions] conj pos))
        neighbor-count (case color
                         :white :white-neighbors
                         :black :black-neighbors)]
    (reduce
      (fn [board n]
        (let [board (-> board
                      (update-in [:positions n :liberties] inc)
                      (update-in [:positions n :neighbor-sum] #(+ % pos))
                      (update-in [:positions n :neighbor-sum-of-squares] #(+ % (* pos pos)))
                      (update-in [:positions n neighbor-count] inc))
              n-color (color board n)]
          (if (= pos-color n-color)
            (let [p (penultimate-parent board n)]
              (update-parent board p p))
            board)))
      board
      (p/neighbors (:dim board) pos))))

;;;

(defn make-random-move [board color]
  (let [ps (:empty-positions board)
        p (nth (seq ps) (rand-int (count ps)))]
    (add-stone board p color)))

(defn playout [dim moves]
  (let [b (empty-board dim)]
    (reduce
      (fn [board _]
        (-> board
          (make-random-move :white)
          (make-random-move :black)))
      b
      (range moves))))
