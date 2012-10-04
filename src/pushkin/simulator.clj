;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns pushkin.simulator
  (:use
    [pushkin.board])
  (:require
    [pushkin.hash :as h]
    [pushkin.position :as p]))

;;;

(defn update-parent [board pos parent]
  (-> board
    (update-in [:positions parent :liberties] #(+ % (liberties board pos)))
    (update-in [:positions parent :neighbor-sum] #(+ % (neighbor-sum board pos)))
    (update-in [:positions parent :neighbor-sum-of-squares] #(+ % (neighbor-sum-of-squares board pos)))
    (assoc-in [:positions pos :parent] parent)))

(defn remove-stone [board pos]
  (let [stone-color (color board pos)
        board (-> board
                (assoc-in [:positions pos :color] :empty)
                (assoc-in [:positions pos :parent] pos)
                (update-in [:empty-positions] conj pos)
                (update-in [:hash] #(h/update-hash % pos stone-color)))
        neighbor-count (case stone-color
                         :white :white-neighbors
                         :black :black-neighbors)]
    (reduce
      (fn [board n]
        (let [p (parent board n)
              board (-> board
                      (update-in [:positions p :liberties] inc)
                      (update-in [:positions p :neighbor-sum] #(+ % pos))
                      (update-in [:positions p :neighbor-sum-of-squares] #(+ % (* pos pos)))
                      (update-in [:positions n neighbor-count] dec))
              n-color (color board n)]
          (if (and (= stone-color n-color) (= pos p))
            (assoc-in board [:positions n :parent] (penultimate-parent board n))
            board)))
      board
      (p/neighbors (:dim board) pos))))

(defn clear-group [board pos]
  (let [g (group board pos)
        score (case (color board pos)
                :white :black-score
                :black :white-score)]
    (reduce
      #(remove-stone %1 %2)
      (update-in board [score] + (count g))
      g)))

(defn add-stone [board pos stone-color]
  (let [dim (:dim board)
        board (-> board
                (assoc-in [:positions pos :neighbor-sum] 0)
                (assoc-in [:positions pos :neighbor-sum-of-squares] 0)
                (assoc-in [:positions pos :liberties] 0)
                (assoc-in [:positions pos :color] stone-color)
                (update-in [:empty-positions] disj pos)
                (update-in [:hash] #(-> % h/rotate-hashes (h/update-hash pos stone-color))))
        neighbor-count (case stone-color
                         :white :white-neighbors
                         :black :black-neighbors)
        board (reduce
                (fn [board n]
                  (let [np (parent board n)
                        n-color (color board n)
                        board (update-in board [:positions n neighbor-count] inc)
                        board (if (not= :empty n-color)
                                (-> board
                                  (update-in [:positions np :liberties] dec)
                                  (update-in [:positions np :neighbor-sum] #(- % pos))
                                  (update-in [:positions np :neighbor-sum-of-squares] #(- % (* pos pos))))
                                (-> board
                                  (update-in [:positions pos :liberties] inc)
                                  (update-in [:positions pos :neighbor-sum] #(+ % n))
                                  (update-in [:positions pos :neighbor-sum-of-squares] #(+ % (* n n)))))
                        board (if (and (= stone-color n-color) (not= pos np))
                                (update-parent board (parent board n) pos)
                                board)]
                    board))
                board
                (p/neighbors (:dim board) pos))]
    (reduce
      (fn [board n]
        (let [n-color (color board n)]
          (if (and (not= :empty n-color) (zero? (liberties board n)))
            (clear-group board n)
            board)))
      board
      (p/neighbors (:dim board) pos))))

;;;

(defn random-move [board positions color]
  (when-not (empty? positions)
    (let [p (nth (seq positions) (rand-int (count positions)))]
      (if (and (not (eye-type board p)) (not= :ko (capture-type board color p)))
        p
        (random-move board (disj positions p) color)))))

(defn playout-game [board color pass? validate?]
  (loop [player color, pass? pass?, board board]
    (when validate?
      (validate-positions board))
    (if-let [move (random-move board (:empty-positions board) player)]
      (recur (opponent player) false (add-stone board move player))
      (if pass?
        (final-score board)
        (recur (opponent player) true board)))))

(defn run-playouts [n board color pass?]
  (->> (range n)
    (map (fn [_] (playout-game board color pass? false)))
    (map (fn [{:keys [white black]}]
           (cond
             (< white black) 1
             (> white black) -1
             :else 0)))
    (apply +)))


