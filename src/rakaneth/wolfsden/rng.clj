(ns rakaneth.wolfsden.rng)

(set! *unchecked-math* true)

(defn count-if [f coll]
  (count (filter f coll)))

(defn sum [coll]
  (reduce + coll))

(defn -shuffle-rng [rng]
  (let [state @rng]
    (as-> state s
      (bit-xor s (bit-shift-left s 13))
      (bit-xor s (bit-shift-right s 17))
      (bit-xor s (bit-shift-left s 5))
      (reset! rng s))
    rng))

(defn new-rng
  "Produces a new RNG instance."
  ([seed]
   (atom seed))
  ([]
   (new-rng (.getEpochSecond (java.time.Instant/now)))))

(def +global-rng+ (new-rng))

(defn next-int
  "Gets a random int in [low, high]"
  ([rng low high]
   (let [rand-max 0xFFFFFFFF
         interval (- (inc high) low)
         rand-limit (- rand-max (mod (inc rand-max) interval))
         shuffled (-shuffle-rng rng)]
     (loop [v @shuffled]
       (if (<= v rand-limit)
         (+ low (mod v interval))
         (recur @(-shuffle-rng shuffled))))))
  ([low high]
   (next-int +global-rng+ low high))
  ([high]
   (next-int +global-rng+ 0 high)))

(defn next-bool
  "Gets true or false, randomly."
  [& {:keys [rng] :or {rng +global-rng+}}]
  (bit-test @(-shuffle-rng rng) 0))

(defn weighted
  "Gets a random item from a table with items as keys mapped to weights."
  [tbl & {:keys [rng] :or {rng +global-rng+}}]
  (if (nil? tbl)
    nil
    (let [s (sum (vals tbl))
          roll (next-int rng 0 (dec s))]
      (loop [lst (seq tbl)
             cur-w (nth (first lst) 1)]
        (if (< roll cur-w)
          (nth (first lst) 0)
          (recur (next lst) (+ cur-w (nth (first (next lst)) 1))))))))

(defn choice
  "Gets a random selection from any seq."
  [coll & {:keys [rng] :or {rng +global-rng+}}]
  (let [l (dec (count coll))
        roll (next-int rng 0 l)]
    (nth coll roll)))

(defn test-fn [x f & args]
  (sort (take x (repeatedly #(apply f args)))))

(defn die-roll
  "Rolls 1dX."
  [sides & {:keys [rng] :or {rng +global-rng+}}]
  (next-int rng 1 sides))

(defn roll-base
  [sides & {:keys [dice kp target diff rng modifier mod-each]
            :or {dice 1
                 kp dice
                 target 0
                 diff 0
                 rng +global-rng+
                 modifier 0
                 mod-each 0}}]
  (let [rolls (sort > (take dice (repeatedly #(+ mod-each (die-roll sides)))))
        k (min kp dice)
        kept (take k rolls)
        hits (count-if #(>= % diff) kept)
        total (+ modifier (sum kept))
        success (>= total target)]
    {:rolls rolls
     :kept kept
     :hits hits
     :total total
     :success success}))

(def +zero-roll+ {:rolls '()
                  :kept '()
                  :hits 0
                  :total 0
                  :success false})

(defn roll-string
  "Parses and rolls a dice string"
  [s & {:keys [rng] :or {rng +global-rng+}}]
  (if (nil? s)
    +zero-roll+
    (let [p #"(\d+)d(\d+)((?:\+|\-)\d+)?(?:k(\d+))?(?:t(\d+))?(?:f(\d+))?(?:e(\d+))?"
          m (re-matches p s)
          [dice sides md kept target diff mde] (map #(if % (Integer/parseInt %) 0) 
                                                    (next m))
          k (if (zero? kept) dice kept)]
      (roll-base sides
                 :dice dice
                 :modifier md
                 :kp k
                 :target target
                 :diff diff
                 :mod-each mde
                 :rng rng))))

(defn add-rolls
  "Adds the results of two rolls together."
  [a b]
  {:rolls (flatten (conj (:rolls a) (:rolls b)))
   :kept (flatten (conj (:kept a) (:kept b)))
   :total (+ (:total a) (:total b))})

(defn rand-bytes
  "Gets 16 random bytes - used in UUID generation"
  [& {:keys [rng] :or {rng +global-rng+}}]
  (into [] (take 16 (repeatedly #(next-int rng 0 255)))))

(defn uuid-4-transform
  "Transforms a list of 16 bytes according to UUID v4 spec"
  [bytes-list]
  (let [new-6 (bit-or 0x40 (bit-and (get bytes-list 6) 0xF))
        new-8 (bit-or 0x80 (bit-and (get bytes-list 8) 0x3F))]
    (-> bytes-list
        (assoc 6 new-6)
        (assoc 8 new-8))))

(defn uuid-4-string
  "Turns a modified UUID vector of bytes into a string"
  [bytes-list]
  (let [base (clojure.string/join (map #(format "%02x" %) bytes-list))]
    (str (subs base 0 8) "-" 
         (subs base 8 12) "-" 
         (subs base 12 16) "-"
         (subs base 16 20) "-"
         (subs base 20))))

(defn uuid 
  "Generates a random v4 UUID"
  [& {:keys [rng] :or {rng +global-rng+}}]
  (let [b (rand-bytes :rng rng)]
    (-> b
        (uuid-4-transform)
        (uuid-4-string))))
