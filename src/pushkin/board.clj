;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns pushkin.board
  (:use
    [pushkin core]
    [useful.datatypes])
  (:require
    [useful.state :as state]
    [pushkin.hash :as h]
    [pushkin.position :as p])
  (:import
    [java.util
     BitSet
     LinkedList]
    [java.util.concurrent.atomic
     AtomicReference
     AtomicInteger]
    [pushkin.position
     Position]))

;;;

(defprotocol IBoard
  (add-white-score [_ n])
  (white-score [_])
  (add-black-score [_ n])
  (black-score [_]))

(defrecord Board
  [^long dim
   ^objects positions
   ^AtomicInteger white-score
   ^AtomicInteger black-score
   hash
   moves]

  ICloneable
  
  (clone [_]
    (Board.
      dim
      (p/clone-positions positions)
      (AtomicInteger. (.get white-score))
      (AtomicInteger. (.get black-score))
      hash
      (atom @moves)))

  
  IBoard

  (white-score [_] (.get white-score))
  (add-white-score [_ n] (.addAndGet white-score n))
  (black-score [_] (.get black-score))
  (add-black-score [_ n] (.addAndGet black-score n)))

(defn dim [^Board board]
  (.dim board))

(defmacro position [board n]
  `(let [^Board board# ~board]
     (p/position (.positions board#) (long ~n))))

(defn available-moves [^Board board]
  @(.moves board))

;;;

(declare color)

(defn ^Board empty-board [dim]
  (make-record Board
    :dim dim
    :positions (p/initial-positions dim)
    :white-score (AtomicInteger. 0)
    :black-score (AtomicInteger. 0)
    :hash (h/zobrist-hash)
    :moves (atom (set (range (* dim dim))))))

(defn print-board [^Board board]
  (let [dim (dim board)
        cols (->> (range (inc dim))
               (map #(char (+ (int \A) %)))
               (remove #{\I})
               (apply str))]
    (println "Black:" (black-score board))
    (println "White:" (white-score board))
    (println (str "  " cols "\n"))
    (doseq [y (reverse (range dim))]
      (print (str (inc y) " "))
      (doseq [x (range dim)]
        (let [c (color (position board (p/coord->position dim x y)))]
          (print
            (case c
              :white "O"
              :black "X"
              :empty "."))))
      (println (str " " (inc y))))
    (println (str "\n  " cols "\n"))))

(defmethod print-method Board [o w]
  (.write ^java.io.Writer w (str (with-out-str (print-board o)))))

;;;

(defmacro def-getter [name field]
  `(defn ~name
     [^Position pos#]
     (~field pos#)))

(defmacro def-setter [name method]
  `(defn ~name
     [^Position pos# value#]
     (~method pos# value#)))

(defmacro def-resetter [name method]
  `(defn ~name
     [^Position pos#]
     (~method pos#)))

(defmacro foreach-neighbor [board [p n] params & body]
  `(let [^Board board# ~board
         ^Position pos# ~p
         pos# (.value pos#)]
     (p/foreach-neighbor (.dim board#) (.positions board#)
       [pos# ~(with-meta n {:tag "pushkin.position.Position"})] ~params
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
      :white (add-black-score board 1)
      :black (add-white-score board 1))

    (h/toggle (.hash board) pos stone-color)

    (set-color pos :empty)
    (set-parent pos pos)
    (reset-liberties pos)
    (reset-neighbor-sum pos)
    (reset-neighbor-sum-of-squares pos)

    ;; todo: capture callback
    (swap! (.moves board) conj (.value pos))

    (let [val (.value pos)]
      (foreach-neighbor board [pos n] []

        ;; update neighbor counts
        (case stone-color
          :black (add-black-neighbors n -1)
          :white (add-white-neighbors n -1))
        
        (let [n-color (color n)]
          (when-not (identical? stone-color n-color)
            
            ;; add empty neighbors to local values
            (when (identical? :empty n-color)
              (let [n-val (.value n)]
                (add-liberties pos 1)
                (add-neighbor-sum pos n-val)
                (add-neighbor-sum-of-squares pos (unchecked-multiply n-val n-val))))
            
            ;; add self to neighbors
            (let [pn (parent board n)]
              (add-liberties pn 1)
              (add-neighbor-sum pn val)
              (add-neighbor-sum-of-squares pn (unchecked-multiply val val)))))))))

(defn clear-group [^Board board ^Position pos]
  (let [stone-color (color pos)]
    (remove-stone board pos)
    (foreach-neighbor board [pos n] []
      (when (identical? stone-color (color n))
        (clear-group board n)))))

(defn add-stone [^Board board ^Position pos stone-color]
  (h/rotate (.hash board))
  (h/toggle (.hash board) pos stone-color)
  (set-color pos stone-color)

  (swap! (.moves board) disj (.value pos))

  ;; update neighbors
  (let [val (.value pos)
        opponent-color (p/opponent stone-color)]
    (foreach-neighbor board [pos n] []

      ;; update neighbor counts
      (case stone-color
        :black (add-black-neighbors n 1)
        :white (add-white-neighbors n 1))
      
      (let [pn (parent board n)
            pn-color (color pn)

            ;; join neighboring groups to this position
            pn (if (identical? stone-color pn-color)
                 (do
                   (join-to-parent pn pos)
                   pos)
                 pn)]

        ;; if this is a pending capture, clear the group out
        (if (and (identical? pn-color opponent-color) (== 1 (liberties pn)))
          (clear-group board pn)

          ;; otherwise, just update the liberty counts
          (do
            (add-liberties pn -1)
            (add-neighbor-sum pn (unchecked-subtract 0 val))
            (add-neighbor-sum-of-squares pn (unchecked-subtract 0 (unchecked-multiply val val)))))))))

;;;

(defn neighbors
  ([board pos]
     (neighbors board pos (constantly true)))
  ([board pos predicate]
     (let [ns (atom [])]
       (foreach-neighbor board [pos n] []
         (when (predicate (color n))
           (swap! ns conj n)))
       @ns)))

(defn num-neighbors [board ^Position pos]
  (p/num-neighbors (.value pos) (dim board)))

(defn atari
  "Returns the numerical position of the atari point for 'pos', nor nil if there is none."
  [^Board board ^Position pos]
  (let [pos (parent board pos)
        liberties (liberties pos)]
    (when (<= liberties 4)
      (let [sum (neighbor-sum pos)
            sum-of-squares (neighbor-sum-of-squares pos)]
        (when (==
                (long (unchecked-multiply (long sum) (long sum)))
                (long (unchecked-multiply (long liberties) (long sum-of-squares))))
          (/ (long sum) (long liberties)))))))

(defn atari? [board pos]
  (boolean (atari board pos)))

(defn capture [board stone-color pos]
  (let [opponent-color (p/opponent stone-color)]
    (foreach-neighbor board [pos n] [captured nil]
      (if captured
        captured
        (when (and 
                (identical? opponent-color (color n))
                (atari? board n))
          n)))))

(defn capture? [board stone-color pos]
  (boolean (capture board stone-color pos)))

(defn capture-all? [board stone-color pos]
  (let [opponent-color (p/opponent stone-color)]
    (foreach-neighbor board [pos n] [capture? true]
      (and capture?
        (if (identical? opponent-color (color n))
          (atari? board n)
          true)))))

(defn eye? [board p]
  (let [num-neighbors (num-neighbors board p)]
    (or
      (and
        (== num-neighbors (white-neighbors p))
        (not (capture? board :black p))
        :white)
      (and
        (== num-neighbors (black-neighbors p))
        (not (capture? board :white p))
        :black))))

(defn ko? [^Board board stone-color p]
  (when-let [n (capture board stone-color p)]
    (and
      (= n (parent board n))
      (case stone-color
        :white (h/ko? (.hash board) p n)
        :black (h/ko? (.hash board) n p)))))

(defn suicide? [board stone-color p]
  (let [num-neighbors (num-neighbors board p)
        num-same-neighbors (case stone-color
                             :white (white-neighbors p)
                             :black (black-neighbors p))
        num-diff-neighbors (case stone-color
                             :white (black-neighbors p)
                             :black (white-neighbors p))]
    (boolean
      (and
        (== num-neighbors (unchecked-add (long num-same-neighbors) (long num-diff-neighbors)))
        (if (pos? num-diff-neighbors)
          (not (capture? board stone-color p))
          true)
        (if (pos? num-same-neighbors)
          (capture-all? board (p/opponent stone-color) p)
          true)))))

(defn final-score [^Board board]
  (merge-with +
    {:white (white-score board)
     :black (black-score board)}
    (->> (.positions board)
      (filter #(= :empty (color %)))
      (map #(eye? board %))
      (remove false?)
      frequencies)))


;;;

(defprotocol IMoveSet
  (stone-added [_ pos color])
  (stone-removed [_ pos])
  (push-position [_ pos])
  (pop-position [_]))

(defrecord MoveSet
  [^BitSet white-atari
   ^BitSet black-atari
   ^BitSet suicide
   ^BitSet eyes])

(defn move-set [board positions]
  )
