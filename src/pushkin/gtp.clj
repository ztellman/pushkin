;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns pushkin.gtp
  (:require
    [duel.core :as duel]
    [pushkin.tree :as t]
    [pushkin.position :as p]))

(defn handler [playouts]
  (let [cnt (atom 0)
        curr-move #(let [curr @cnt]
                     (swap! cnt inc)
                     curr)
        dim (atom 9)
        node (atom (t/starting-node 9))]
    (fn [[cmd & args]]
      (case cmd
        
        :genmove
        (time
          (do
            (->>
              (range 8)
              (map (fn [_]
                     (future
                       (dotimes [_ playouts]
                         (t/traverse @node)))))
              doall
              (map deref)
              doall)
            (let [move (t/gen-move @node (curr-move))]
              (p/position->gtp move @dim))))

        :play
        (let [[color position] args
              position (p/gtp->position position @dim)]
          (t/set-move @node (curr-move) position)
          "")

        ""))))

(defn gnugo-playout [playouts]
  (duel/run-trials
    #(duel/create-internal-player (handler playouts))
    #(duel/gnugo-player :level 1)))


