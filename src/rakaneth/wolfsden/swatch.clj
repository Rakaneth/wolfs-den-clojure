(ns rakaneth.wolfsden.swatch
  (:import [java.awt Color]))

(def swatch {:sepia (Color. 191 171 143)
             :dark-sepia (Color. 158 134 100)
             :darker-sepia (Color. 127 101 63)
             :cyan (Color. 0 255 255)
             :white (Color. 255 255 255)
             :black-mark (Color. 63 63 63)
             :red-mark (Color. 191 74 0)
             :blue-mark (Color. 0 127 255)
             :green-mark (Color. 0 191 0)
             :stone (Color. 100 100 100)
             :dark-stone (Color. 50 50 50)
             :black (Color. 0 0 0)
             :transparent (Color. 0 0 0 0)})

(defn color-by-name [color-key]
  (or (get swatch color-key) 
      (throw (IllegalArgumentException. (str  "No color named " color-key)))))
