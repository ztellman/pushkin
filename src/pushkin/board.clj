;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns pushkin.board
  (:require
    [pushkin.hash :as h]
    [pushkin.position :as p]))

;;;

(defrecord Board
  [dim
   positions
   empty-positions
   white-score
   black-score
   hash]
  Object
  (toString [_]
    "** BOARD **"))

(def empty-board
  (memoize
    (fn [dim]
      (let [ps (range (* dim dim))]
        (map->Board
          {:dim dim
           :eyes #{}
           :empty-positions (set ps)
           :positions (vec (map #(p/initial-position % dim) ps))
           :white-score 0
           :black-score 0
           :hash (h/zobrist-hash)})))))

(defn print-board [board]
  (let [dim (:dim board)
        cols (->> (range (inc dim))
               (map #(char (+ (int \A) %)))
               (remove #{\I})
               (apply str))]
    (println "Black:" (:black-score board))
    (println "White:" (:white-score board))
    (println (str "  " cols "\n"))
    (doseq [y (reverse (range dim))]
      (print (str (inc y) " "))
      (doseq [x (range dim)]
        (let [c (get-in board [:positions (p/position dim x y) :color])]
          (print
            (case c
              :white "O"
              :black "X"
              :empty "."))))
      (println (str " " (inc y))))
    (println (str "\n  " cols "\n"))))

(defmethod print-method Board [o ^java.io.Writer w]
  (.write w (str o)))

;;;

(defmacro def-accessor [name field]
  `(defn ~name [board# pos#]
     (get-in board# [:positions pos# ~field])))

(declare parent color)

(defmacro def-parent-accessor [name field]
  `(defn ~name [board# pos#]
     (get-in board# [:positions (parent board# pos#) ~field])))

(defn neighbors
  ([board pos]
     (neighbors board pos (constantly true)))
  ([board pos predicate]
     (->> (p/neighbors pos (:dim board))
       (filter #(predicate (color board %))))))

(defn group
  ([board n]
     (if (= :empty (color board n))
       #{n}
       (group board n #{})))
  ([board n group-set]
     (if (group-set n)
       group-set
       (let [group-set (conj group-set n)]
         (reduce
           #(group board %2 %1)
           group-set
           (neighbors board n #{(color board n)}))))))

(defn position-range [board]
  (let [dim (:dim board)]
    (range (* dim dim))))

;;;

(def-accessor color :color)
(def-parent-accessor liberties :liberties)
(def-accessor white-neighbors :white-neighbors)
(def-accessor black-neighbors :black-neighbors)
(def-accessor local-parent :parent)
(def-parent-accessor neighbor-sum :neighbor-sum)
(def-parent-accessor neighbor-sum-of-squares :neighbor-sum-of-squares)

(defn update-color [board pos color]
  (assoc-in board [:positions pos :color] color))

(defn parent [board pos]
  (loop [pos pos]
    (let [p (local-parent board pos)]
      (if (= p pos)
        p
        (recur p)))))

(defn penultimate-parent [board pos]
  (let [n (local-parent board pos)
        nn (local-parent board n)]
    (loop [a pos, b n, c nn]
     (if (= b c)
       a
       (recur b c (local-parent board c))))))

(defn atari? [board pos]
  (let [sum (neighbor-sum board pos)
        sum-of-squares (neighbor-sum-of-squares board pos)]
    (= (* sum sum) (* (liberties board pos) sum-of-squares))))

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
      (p/neighbors pos (:dim board)))))

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
                (p/neighbors pos (:dim board)))
        board (reduce
                (fn [board n]
                  (let [n-color (color board n)]
                    (if (and (not= :empty n-color) (zero? (liberties board n)))
                      (clear-group board n)
                      board)))
                board
                (neighbors board pos #{(p/opponent stone-color)}))]
    board))

;;;

(defn capture? [board color p]
  (->> (neighbors board p #{(p/opponent color)})
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
      (= n (parent board n))
      (-> board
        :hash
        h/rotate-hashes
        (h/update-hash p color)
        (h/update-hash n (p/opponent color))
        h/cycle?))))

(defn suicide? [board color p]
  (let [num-neighbors (count (p/neighbors p (:dim board)))
        same-neighbors (neighbors board p #{color})
        diff-neighbors (neighbors board p #{(p/opponent color)})]
    (and
      (= num-neighbors (+ (count same-neighbors) (count diff-neighbors)))
      (not (some #(atari? board %) diff-neighbors))
      (every? #(atari? board %) same-neighbors))))

(defn final-score [board]
  (merge-with +
    {:white (:white-score board)
     :black (:black-score board)}
    (->> (position-range board)
      (filter #(= :empty (color board %)))
      (map #(eye? board %))
      (remove false?)
      frequencies)))
