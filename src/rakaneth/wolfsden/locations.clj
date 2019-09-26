(ns rakaneth.wolfsden.locations)
(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(deftype Location [^int x ^int y]
  clojure.lang.Indexed
  (nth [loc i]
    (let [li (long i)]
      (case li
        0 (.x loc)
        1 (.y loc)
        (throw (IndexOutOfBoundsException. (str "index: " i))))))
  (nth [loc i not-found]
    (let [li (long i)]
      (case li
        0 (.x loc)
        1 (.y loc)
        not-found)))
  clojure.lang.IFn
  (invoke [loc i]
    (let [li (long i)]
      (case li
        0 (.x loc)
        1 (.y loc)
        (throw (IndexOutOfBoundsException. (str "index: " i))))))
  clojure.lang.ILookup
  (valAt [loc i]
    (let [li (long i)]
      (case li
        0 (.x loc)
        1 (.y loc)
        (throw (IndexOutOfBoundsException. (str "index: " i))))))
  (valAt [loc i not-found]
    (let [li (long i)]
      (case li
        0 (.x loc)
        1 (.y loc)
        not-found)))
  java.lang.Object
  (hashCode [a]
    (+ (.x a) (bit-shift-left (.y a) 5)))
  (equals [a b]
    (if (instance? Location b)
      (let [^Location b b]
        (and (== (.x a) (.x b)) (== (.y a) (.y b))))
      false))
  (toString [this]
    (str "(" (.x this) "," (.y this) ")")))

(defn loc 
  "Location constructor"
  ([xs]
   (->Location (xs 0) (xs 1)))
  ([^long x ^long y]
   (->Location x y)))

(def +directions+ {:N (loc 0 -1)
                   :NE (loc 1 -1)
                   :E (loc 1 0)
                   :SE (loc 1 1)
                   :S (loc 0 1)
                   :SW (loc -1 1)
                   :W (loc -1 0)
                   :NW (loc -1 -1)
                   :C (loc 0 0)})

(defn loc-add
  "Transposes loc by another loc or dx dy."
  ([^Location a ^Location b]
   (loc (+ (.x a) (.x b)) (+ (.y a) (.y b))))
  ([^Location a ^long dx ^long dy]
   (loc (+ (.x a) dx) (+ (.y a) dy))))

(defn loc-move-dir
  "Returns a loc transposed by a named direction."
  [l kw]
  (loc-add l (kw +directions+)))

(defn loc? 
  "Returns true if obj is a location."
  [obj]
  (instance? Location obj))

(defn c-distance 
  "Kings-move distance from a to b."
  [^Location a ^Location b]
  (max (Math/abs (- (.x a) (.x b)))
       (Math/abs (- (.y a) (.y b)))))

(defn e-distance
  "Euclidean distance from a to b."
  [^Location a ^Location b]
  (let [dx (- (.x a) (.x b))
        dy (- (.y a) (.y b))]
    (Math/sqrt (+ (* dx dx) (* dy dy)))))

(defn m-distance
  "Manhattan distance from a to b."
  [^Location a ^Location b]
  (let [dx (- (.x a) (.x b))
        dy (- (.y a) (.y b))]
    (+ (Math/abs dx) (Math/abs dy))))

(defn adj?
  "Returns true if a and b are adjacent."
  [^Location a ^Location b]
  (= (c-distance a b) 1))


