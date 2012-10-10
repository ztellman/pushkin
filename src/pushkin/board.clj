;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns pushkin.board
  (:use
    [useful.datatypes])
  (:require
    [pushkin.hash :as h]
    [pushkin.position :as p])
  (:import
    [pushkin.position
     Position]))

;;;

(definterface IBoard
  (clone [])
  (^long white_score [])
  (^long add_white_score [^long n])
  (^long black_score [])
  (^long add_black_score [^long n]))

(deftype Board
  [^long dim
   ^objects positions
   ^:unsynchronized-mutable ^long white-score
   ^:unsynchronized-mutable ^long black-score
   hash
   moves]

  IBoard

  (clone [_]
    (Board.
      dim
      (p/clone-positions positions)
      white-score
      black-score
      hash
      (atom @moves)))

  (^long white_score [_] white-score)
  (^long add_white_score [_ ^long n] (set! white-score (unchecked-add white-score n)))

  (^long black_score [_] black-score)
  (^long add_black_score [_ ^long n] (set! black-score (unchecked-add black-score n))))

(defn dim [^Board board]
  (.dim board))

(defmacro position [board n]
  `(let [^Board board# ~board]
     (p/position (.positions board#) (long ~n))))

(defn available-moves [^Board board]
  @(.moves board))

;;;

(defn ^Board empty-board [dim]
  (make-record Board
    :dim dim
    :positions (p/initial-positions dim)
    :white-score 0
    :black-score 0
    :hash (h/zobrist-hash)
    :moves (atom (set (range (* dim dim))))))

(defn print-board [^Board board]
  (let [dim (dim board)
        cols (->> (range (inc dim))
               (map #(char (+ (int \A) %)))
               (remove #{\I})
               (apply str))]
    (println "Black:" (.black_score board))
    (println "White:" (.white_score board))
    (println (str "  " cols "\n"))
    (doseq [y (reverse (range dim))]
      (print (str (inc y) " "))
      (doseq [x (range dim)]
        (let [c (.color ^Position (position board (p/coord->position dim x y)))]
          (print
            (case c
              :white "O"
              :black "X"
              :empty "."))))
      (println (str " " (inc y))))
    (println (str "\n  " cols "\n"))))

(defmethod print-method Board [o ^java.io.Writer w]
  (.write w (with-out-str (print-board o))))

;;;

(declare parent color)

(defmacro def-getter [name field]
  `(defn ~(with-meta name {:tag Long/TYPE})
     {:inline (fn [pos#] (list '~field pos#))
      :inline-arities #{1}}
     [^Position pos#]
     (~field pos#)))

(defmacro def-setter [name method]
  `(defn ~(with-meta name {:tag Long/TYPE})
     [^Position pos# value#]
     (~method pos# value#)))

(defmacro def-resetter [name method]
  `(defn ~name
     {:inline (fn [pos#] (list '~method pos#))
      :inline-arities #{1}}
     [^Position pos#]
     (~method pos#)))

(defmacro foreach-neighbor [board [p n] & body]
  `(let [^Board board# ~board
         ^Position pos# ~p
         pos# (.value pos#)]
     (p/foreach-neighbor (.dim board#) (.positions board#) [pos# ~n]
       ~@body)))

;;;

(defn color [^Position pos]
  (.color pos))

(defn set-color [^Position pos value]
  (.set_color pos value))

(defn set-parent [^Position pos ^Position parent]
  (.set_parent pos (.value parent)))

(def-getter white-neighbors .white_neighbors)
(def-getter black-neighbors .black_neighbors)
(def-setter add-white-neighbors .add_white_neighbors)
(def-setter add-black-neighbors .add_black_neighbors)

(def-getter liberties .liberties)
(def-setter add-liberties .add_liberties)
(def-resetter reset-liberties .reset_liberties)

(def-getter neighbor-sum .neighbor_sum)
(def-setter add-neighbor-sum .add_neighbor_sum)
(def-resetter reset-neighbor-sum .reset_neighbor_sum)

(def-getter neighbor-sum-of-squares .neighbor_sum_of_squares)
(def-setter add-neighbor-sum-of-squares .add_neighbor_sum_of_squares)
(def-resetter reset-neighbor-sum-of-squares .reset_neighbor_sum_of_squares)

(defn parent [^Board board ^Position pos]
  (let [parent (.parent pos)]
    (if (= (.value pos) parent)
      pos
      (let [origin pos]
        (loop [pos (long parent)]
          (let [^Position pos (position board pos)
                parent (.parent pos)]
            (if (= parent (.value pos))
              (do (set-parent origin pos) pos)
              (recur parent))))))))

(defn atari? [^Board board ^Position pos]
  (let [pos (parent board pos)
        sum (neighbor-sum pos)
        sum-of-squares (neighbor-sum-of-squares pos)]
    (=
      (unchecked-multiply-int sum sum)
      (unchecked-multiply-int (liberties pos) sum-of-squares))))

;;;

(defn join-to-parent [^Position pos ^Position parent]
  (when-not (identical? pos parent)
    (set-parent pos parent)
    (add-liberties parent (liberties pos))
    (add-neighbor-sum parent (neighbor-sum pos))
    (add-neighbor-sum-of-squares parent (neighbor-sum-of-squares pos))))

(defn remove-stone [^Board board ^Position pos]
  (let [stone-color (color pos)]

    (case stone-color
      :white (.add_black_score board 1)
      :black (.add_white_score board 1))

    (h/toggle (.hash board) pos stone-color)

    (assert (not= :empty (color pos)))

    (set-color pos :empty)
    (set-parent pos pos)
    (reset-liberties pos)
    (reset-neighbor-sum pos)
    (reset-neighbor-sum-of-squares pos)

    ;; todo: capture callback
    (swap! (.moves board) conj (.value pos))

    (let [val (.value pos)]
      (foreach-neighbor board [pos n]

        (case stone-color
          :black (add-black-neighbors n -1)
          :white (add-white-neighbors n -1))
        
        (when-not (identical? stone-color (color n))

          (when (identical? :empty (color n))
            (let [n-val (.value n)]
              (add-liberties pos 1)
              (add-neighbor-sum pos n-val)
              (add-neighbor-sum-of-squares pos (unchecked-multiply-int n-val n-val))))

          (let [pn (parent board n)]
            (add-liberties pn 1)
            (add-neighbor-sum pn val)
            (add-neighbor-sum-of-squares pn (unchecked-multiply-int val val))))))))

(defn clear-group [^Board board ^Position pos]
  (let [stone-color (color pos)]
    (remove-stone board pos)
    (foreach-neighbor board [pos n]
      (when (identical? stone-color (color n))
        (clear-group board n)))))

(defn add-stone [^Board board ^Position pos stone-color]
  (h/rotate (.hash board))
  (h/toggle (.hash board) pos stone-color)
  (set-color pos stone-color)

  (swap! (.moves board) disj (.value pos))

  ;; update neighbors
  (let [val (.value pos)]
    (foreach-neighbor board [pos n]
      
      (case stone-color
        :black (add-black-neighbors n 1)
        :white (add-white-neighbors n 1))
      
      (let [pn (parent board n)
            pn (if (identical? stone-color (color pn))
                 (do (join-to-parent pn pos) pos)
                 pn)]
        (add-liberties pn -1)
        (add-neighbor-sum pn (- val))
        (add-neighbor-sum-of-squares pn (- (unchecked-multiply-int val val))))))

  ;; check for captures
  (let [opponent-color (p/opponent stone-color)]
    (foreach-neighbor board [pos n]
      (when (identical? opponent-color (color n))
        (let [pn (parent board n)]
          (when (zero? (liberties pn))
            (clear-group board n)))))))

;;;

(defn neighbors
  ([board pos]
     (neighbors board pos (constantly true)))
  ([board pos predicate]
     (let [ns (atom [])]
       (foreach-neighbor board [pos n]
         (when (predicate (color n))
           (swap! ns conj n)))
       @ns)))

(defn capture? [board color p]
  (->> (neighbors board p #{(p/opponent color)})
    (filter #(atari? board %))
    first))

(defn eye? [board p]
  (let [num-neighbors (count (neighbors board p))]
    (or
      (and
        (= num-neighbors (white-neighbors p))
        (not (capture? board :black p))
        :white)
      (and
        (= num-neighbors (black-neighbors p))
        (not (capture? board :white p))
        :black))))

(defn ko? [board color p]
  (when-let [n (capture? board color p)]
    (and
      (= n (parent board n))
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

(defn final-score [^Board board]
  (merge-with +
    {:white (.white_score board)
     :black (.black_score board)}
    (->> (.positions board)
      (filter #(= :empty (color %)))
      (map #(eye? board %))
      (remove false?)
      frequencies)))
