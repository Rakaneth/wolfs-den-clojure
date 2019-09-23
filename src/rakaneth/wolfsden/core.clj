(ns rakaneth.wolfsden.core
  (:gen-class)
  (:require [mikera.orculje.gui :as gui])
  (:import [javax.swing JFrame JComponent KeyStroke])
  (:import [java.awt Font Color])
  (:import [java.awt.event KeyEvent])
  (:import [mikera.gui JConsole]))


(def ^Font font (Font. "Courier New" Font/PLAIN 16))
(def +screen-width+ 100)
(def +screen-height+ 40)

(defn new-frame
  (^JFrame []
   (let [frame (JFrame.)]
     frame)))

(defn new-console
  (^JConsole []
   (let [jc (JConsole.  +screen-width+ +screen-height+)]
     (.setMainFont jc font)
     (.setFocusable jc true)
     (.setCursorVisible jc false)
     (.setCursorBlink jc false)
     jc)))

(defn write-center [^JConsole jc y text]
  (gui/draw jc (quot (- +screen-width+ (count text)) 2) y text))

(defn redraw-screen [state]
  (let [^JConsole jc (:console state)
        screens (:screens state)]
    (doseq [screen screens]
      ((:on-render screen) jc (:game state)))))

(defn new-title-screen []
  {:on-render (fn [^JConsole jc game]
                (write-center jc 20 "Wolf's Den: Clojure Edition")
                (write-center jc 21 "By Rakaneth"))})

(defn new-state []
  (let [state {:game {}
               :console (new-console)
               :frame (new-frame)
               :input nil
               :event-handler nil
               :screens (vector (new-title-screen))}]
    state))

(defn main [state]
  (let [^JFrame frame (:frame state)
        ^JConsole jc (:console state)]
    ;;(setup-input jc state)
    (.add (.getContentPane frame) jc)
    (.pack frame)
    (.setVisible frame true)
    (redraw-screen state)
    frame))

(defn -main
  "Entry point for the app."
  [& args]
  (let [^JFrame frame (main (new-state))]
    (.setDefaultCloseOperation frame JFrame/EXIT_ON_CLOSE)))
