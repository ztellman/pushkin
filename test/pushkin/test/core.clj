(ns pushkin.test.core
  (:require
    [criterium.core]))

(defmacro bench [name & body]
  `(do
     (println "\n-----\n" ~name "\n-----\n")
     (criterium.core/quick-bench
       (do ~@body)
       :reduce-with #(and %1 %2))))

(defn num-cores []
  (.availableProcessors (Runtime/getRuntime)))

(defmacro dotimes-p [[iterator cnt] & body]
  `(let [s# (partition-all ~(num-cores) (range ~cnt))]
     (->> s#
       (map (fn [chunk#]
              (future
                (doseq [~iterator chunk#]
                  ~@body))))
       doall
       (map deref)
       doall)
     nil))
