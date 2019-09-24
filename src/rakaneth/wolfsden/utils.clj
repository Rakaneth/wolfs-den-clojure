(ns rakaneth.wolfsden.utils)

(defn compare-by [coll comp-fn key-fn]
  (reduce (fn [fst snd]
            (if (comp-fn (key-fn fst) (key-fn snd))
              fst
              snd))
          coll))

(defn max-by [coll key-fn]
  (compare-by coll > key-fn))

(defn min-by [coll key-fn]
  (compare-by coll < key-fn))
