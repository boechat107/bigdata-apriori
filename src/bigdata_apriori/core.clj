(ns bigdata-apriori.core
    (:require
      [clojure.math.combinatorics :as comb]
      [clojure.set :as s]
      )
    )

(defn- inc-item-count
  "Updates the count of a item k in the map m if k occurs in fs. 
  fs is a collection of sets."
  [fs m k]
  {:pre [(every? set? fs) (map? m) (set? k)]}
  (when (or (empty? fs)
            ;; Equivalent of the function contains? for a list of sets (contains?
            ;; does not work for this case).
            (->> (filter #(= k %) fs) seq))
    (if (contains? m k)
      (update-in m [k] inc)
      (assoc m k 1))))

(defn- dissoc-infreq
  "Remove the infrequent keys from the given map. The frequency is given by the
  support value."
  [m sup]
  {:pre [(map? m)]}
  (reduce #(if (< (get %1 %2) sup) (dissoc %1 %2) %1)
          m
          (keys m)))

(defn- sets-from-combinatons
  "Returns a collection of sets representing all possible combinations of n of the
  given keys. Each set is possible combination."
  [ks n]
  (->> (comb/combinations ks n) 
       (map #(apply s/union %))))

(defn apriori
  "support is a double value, representing the percentage of the baskets.
  data is the list of baskets."
  [support data]
  (let [sup-val (* support (count data))]
    (loop [set-size 1
           freq-tab nil]
      (let [freq-sets (sets-from-combinatons (keys freq-tab) set-size) 
            ctab (loop [[basket & r-baskets] data
                        counter-tab {} ; table with the count of each set of elements.
                        ]
                   (if basket
                     (recur r-baskets
                            (reduce #(->> %2 set (inc-item-count freq-sets %1)) 
                                    counter-tab
                                    (comb/combinations basket set-size)))
                     counter-tab))
            most-freq (dissoc-infreq ctab sup-val)
            ]
        (if (seq most-freq)
          (recur (inc set-size) most-freq)
          freq-tab)
        ))))
