(ns rakaneth.wolfsden.screens
  (:require [mikera.orculje.gui :as gui])
  (:require [rakaneth.wolfsden.core :as core])
  (:require [rakaneth.wolfsden.utils :as utils])
  (:import [mikera.gui JConsole]))


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
        game (:game st)
        ^JConsole jc (:console st)]
    (if enter-fn
      (enter-fn jc game))
    (println (str "Entering " (:name screen) " screen."))
    (swap! state 
           (fn [cur-state] 
             (assoc cur-state :screen-stack (into [] (conj cur-stack screen)))))))

(defn pop-screen [state]
  (let [st @state
        cur-stack (:screen-stack st)
        leaving (last cur-stack)
        exit-fn (:on-exit leaving)
        ^JConsole jc (:console st)
        game (:game st)]
    (if exit-fn
      (exit-fn jc game))
    (println (str "Exiting " (:name leaving) " screen."))
    (swap! state
           (fn [cur-state]
             (assoc cur-state :screen-stack (into [] (pop cur-stack)))))))

(defn write-center [^JConsole jc y text]
  (gui/draw jc (quot (- core/+screen-width+ (count text)) 2) y text))

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
                      (gui/draw jc (inc x) row (first lst))
                      (recur (inc row) (next lst))))))
   :name (str name "-menu")})

(defn new-title-screen []
  {:on-render (fn [^JConsole jc state]
                (write-center jc 20 "Wolf's Den: Clojure Edition")
                (write-center jc 21 "By Rakaneth"))
   :name "title"
   :on-input (fn [k]
               (case k
                 "enter" (do (push-screen core/s (new-menu 50 20 
                                                           "new-game" 
                                                           "New Game" 
                                                           ["New Game" "Continue"]))
                             (core/redraw-screen @core/s)
                             :handled)
                 :else false))})
