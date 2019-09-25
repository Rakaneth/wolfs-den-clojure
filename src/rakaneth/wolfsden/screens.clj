(ns rakaneth.wolfsden.screens
  (:require [mikera.orculje.gui :as gui])
  (:require [rakaneth.wolfsden.utils :as utils])
  (:require [rakaneth.wolfsden.swatch :as swatch])
  (:import [mikera.gui JConsole])
  (:import [java.awt Color]))

(def +screen-width+ 100)
(def +screen-height+ 40)

(defn draw-box [^JConsole jc x y w h]
  (let [horz (char 0x2550)
        vert (char 0x2551)
        ul (char 0x2554)
        ur (char 0x2557)
        ll (char 0x255A)
        lr (char 0x255D)
        x2 (+ x w)
        y2 (+ y h)
        x2e (dec x2)
        y2e (dec y2)]
    (doseq [xs (range (inc x) x2)]
      (gui/draw jc xs y horz)
      (gui/draw jc xs y2e horz))
    (doseq [ys (range (inc y) y2)]
      (gui/draw jc x ys vert)
      (gui/draw jc x2e ys vert))
    (doseq [[cx cy ch] [[x y ul]
                        [x2e y ur]
                        [x y2e ll]
                        [x2e y2e lr]]]
      (gui/draw jc cx cy ch))))

(defn push-screen [state screen]
  (let [st @state
        cur-stack (:screen-stack st)
        enter-fn (:on-enter screen)
        ^JConsole jc (:console st)]
    (if enter-fn
      (enter-fn jc state))
    (println (str "Entering " (:name screen) " screen."))
    (swap! state 
           (fn [cur-state] 
             (assoc cur-state :screen-stack (into [] (conj cur-stack screen)))))))

(defn pop-screen [state]
  (let [st @state
        cur-stack (:screen-stack st)
        leaving (last cur-stack)
        exit-fn (:on-exit leaving)
        ^JConsole jc (:console st)]
    (if exit-fn
      (exit-fn jc state))
    (println (str "Exiting " (:name leaving) " screen."))
    (swap! state
           (fn [cur-state]
             (assoc cur-state :screen-stack (into [] (pop cur-stack)))))))

(defn redraw-screen [state]
  (let [st @state
        ^JConsole jc (:console st)
        screens (:screen-stack st)]
    (.setBackground jc (swatch/color-by-name :black))
    (.clear jc)
    (doseq [screen screens]
      ((:on-render screen) jc st))))

(defn update-selected [state n]
  (println state)
  (swap! state
         (fn [cur-state]
           (assoc cur-state :selected n)))
  (redraw-screen state))

(defn write-center [^JConsole jc y text]
  (gui/draw jc (quot (- +screen-width+ (count text)) 2) y text))

(defn new-menu [x y name caption select-fn options]
  {:on-render (fn [^JConsole jc state]
                (let [longest (utils/max-by (conj options caption) count)
                      long-len (count longest)
                      w (+ 2 long-len)
                      h (+ 2 (count options))]
                  (draw-box jc x y w h)
                  (when caption
                    (gui/draw jc (inc x) y caption))
                  (loop [row (inc y)
                         lst options]
                    (when-not (= row (+ y h))
                      (if (= (:selected state) (- row y 1))
                        (.setForeground jc (swatch/color-by-name :cyan))
                        (.setForeground jc (swatch/color-by-name :white)))
                      (gui/draw jc (inc x) row (first lst))
                      (recur (inc row) (next lst))))))
   :on-enter (fn [^JConsole jc state] 
               (update-selected state 0))
   :on-exit (fn [^JConsole jc state]
              (update-selected state nil))
   :name (str name "-menu")
   :on-input (fn [state k]
               (let [sel (:selected @state)]
                 (case k 
                   "num-8" (do (update-selected state 
                                                (mod (dec sel) 
                                                     (count options)))
                               :handled)
                   "num-2" (do (update-selected state
                                                (mod (inc sel)
                                                     (count options)))
                               :handled)
                   "enter" (do (select-fn (options sel))
                               :handled))))})

(defn new-title-screen []
  {:on-render (fn [^JConsole jc state]
                (write-center jc 20 "Wolf's Den: Clojure Edition")
                (write-center jc 21 "By Rakaneth"))
   :name "title"
   :on-input (fn [state k]
               (case k
                 "enter" (do (push-screen state (new-menu 50 20 
                                                          "new-game" 
                                                          "New Game"
                                                          #(println "Selected: " %)
                                                          ["New Game" "Continue"]))
                             (redraw-screen state)
                             :handled)
                 :else false))})
