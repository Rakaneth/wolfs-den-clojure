(ns rakaneth.wolfsden.core
  (:gen-class)
  (:require [mikera.orculje.gui :as gui])
  (:require [rakaneth.wolfsden.screens :as scr])
  (:import [javax.swing JFrame JComponent KeyStroke])
  (:import [java.awt Font Color])
  (:import [java.awt.event KeyEvent])
  (:import [mikera.gui JConsole]))


(def ^Font font (Font. "Courier New" Font/PLAIN 16))

(defn new-frame
  (^JFrame []
   (let [frame (JFrame.)]
     frame)))

(defn new-console
  (^JConsole []
   (let [jc (JConsole.  scr/+screen-width+ scr/+screen-height+)]
     (.setMainFont jc font)
     (.setFocusable jc true)
     (.setCursorVisible jc false)
     (.setCursorBlink jc false)
     jc)))

(defn new-state []
  (let [state {:game {}
               :console (new-console)
               :frame (new-frame)
               :selected nil
               :screen-stack (vector)}]
    state))

(defn make-input-action [state k]
  (fn []
    (let [screens (:screen-stack @state)
          screen (last screens)
          handler-fn (:on-input screen)]
      (if-not (and handler-fn (handler-fn state k))
        (println (str "Unhandled key " k " pressed."))))))

(defn setup-input
  [^JComponent comp state]
  (doseq [k "abcdefghijklmnopqrstuvwxyz "]
    (gui/add-input-binding comp (gui/keystroke k) (make-input-action state (str k))))
  (doseq [k "ABCDEFGHIJKLMNOPQRSTUWXYZ"]
    (gui/add-input-binding comp (gui/keystroke k) (make-input-action state (str k))))
  (doseq [[^KeyEvent ke k] {KeyEvent/VK_NUMPAD4 "num-4"
                            KeyEvent/VK_NUMPAD7 "num-7"
                            KeyEvent/VK_NUMPAD8 "num-8"
                            KeyEvent/VK_NUMPAD9 "num-9"
                            KeyEvent/VK_NUMPAD6 "num-6"
                            KeyEvent/VK_NUMPAD3 "num-3"
                            KeyEvent/VK_NUMPAD2 "num-2"
                            KeyEvent/VK_NUMPAD1 "num-1"
                            KeyEvent/VK_1 "1"
                            KeyEvent/VK_2 "2"
                            KeyEvent/VK_3 "3"
                            KeyEvent/VK_4 "4"
                            KeyEvent/VK_5 "5"
                            KeyEvent/VK_6 "6"
                            KeyEvent/VK_7 "7"
                            KeyEvent/VK_8 "8"
                            KeyEvent/VK_9 "9"
                            KeyEvent/VK_0 "0"
                            KeyEvent/VK_ENTER "enter"}]
    (gui/add-input-binding comp (gui/keystroke-from-keyevent ke) (make-input-action state (str k)))))

(def s (atom (new-state)))

(defn reset-game-state! [state]
  (reset! state (new-state)))

(defn main [state]
  (reset-game-state! state)
  (scr/push-screen state (scr/new-title-screen))
  (let [st @state
        ^JConsole jc (:console st)
        ^JFrame frame (:frame st)]
    (setup-input jc state)
    (.add (.getContentPane frame) jc)
    (.pack frame)
    (.setVisible frame true)
    (scr/redraw-screen state)
    frame))

(defn -main
  "Entry point for the app."
  [& args]
  (let [^JFrame frame (main (atom (new-state)))]
    (.setDefaultCloseOperation frame JFrame/EXIT_ON_CLOSE)))
