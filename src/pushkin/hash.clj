;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns pushkin.hash
  (:import
    [java.math
     BigInteger]
    [java.util
     Random]))

(def hash-bits 128)

(defn bigint-xor [^BigInteger a ^BigInteger b]
  (.xor a b))

(let [rnd (Random.)]
  (def black-hashes (vec (repeatedly 361 #(BigInteger. hash-bits rnd))))
  (def white-hashes (vec (repeatedly 361 #(BigInteger. hash-bits rnd))))
  (def initial (BigInteger/valueOf 0)))

(defprotocol ZobristHash
  (rotate-hashes [_])
  (update-hash [_ position color])
  (cycle? [_]))

(defn zobrist-hash
  ([]
     (zobrist-hash [initial initial initial]))
  ([hashes]
     (let [update #(let [[a b c] hashes]
                     [(bigint-xor a %) b c])]
       (reify

         clojure.lang.IDeref
         (deref [_] (first hashes))

         ZobristHash
         (rotate-hashes [this]
           (let [[a b c] hashes]
             (zobrist-hash [a a b])))
         (update-hash [this position color]
           (zobrist-hash
             (case color
               :white (update (nth white-hashes position))
               :black (update (nth black-hashes position)))))
         (cycle? [_]
           (let [[a _ c] hashes]
             (= a c)))))))

