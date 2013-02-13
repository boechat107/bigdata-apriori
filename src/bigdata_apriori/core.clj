(ns bigdata-apriori.core
    (:require
      [clojure.math.combinatorics :as comb]
      [clojure.set :as s]
      )
    )

(defn- inc-item-count
  "Updates the count of a item k in the map m if k occurs in freq-items. 
  freq-items is a collection of sets."
  [freq-items m k]
  {:pre [(every? set? freq-items) (map? m) (set? k)]}
  (when (or (nil? freq-items)
            ;; Equivalent of the function contains? for a list of sets (contains?
            ;; does not work for this case).
            (->> (filter #(= k %) freq-items) seq))
    (if (contains? m k)
      (update-in m [k] inc)
      (assoc m k 1))))


(defn apriori
  "support is a double value, representing the percentage of the baskets.
  data is the list of baskets."
  [support data]
  (let [sup-val (* support (count data))]
    (loop [[basket & r-baskets] data
           must-count nil ; coll of sets whose elements are frequent.
           counter-tab {} ; table with the count of each set of elements.
           n-set 1] ; number of elements of a set.
      (if basket
        (recur r-baskets
               must-count
               (reduce #(->> %2 set (inc-item-count must-count %1)) 
                       counter-tab
                       (comb/combinations basket n-set))
               n-set)
        (let [most-freq (map key (filter #(> (val %) sup-val) counter-tab))
              freq-sets (->> (comb/combinations most-freq (inc n-set))
                             (map #(apply s/union %)))]
          (swank.core/break)
          (if (seq freq-sets)
            (recur data 
                   freq-sets
                   {}
                   (inc n-set))
            most-freq))))))
