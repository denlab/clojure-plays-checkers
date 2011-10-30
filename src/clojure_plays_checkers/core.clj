(ns clojure-plays-checkers.core
  (:use     [midje.sweet])
  (:use     [clojure.pprint :only [pprint]])
  (:use     [clojure.walk   :only [macroexpand-all]])
  (:require [clojure.set     :as set])
  (:import (java.text SimpleDateFormat))
  (:import (java.util Date)))

(unfinished io-show-goodbye io-play-again? io-show-winner
            io-choose-player io-show-welcome game-over?
            new-board-at-startup )

(defn one-game "Loop which will organise one game"
  [] (let [b (new-board-at-startup)]
       (if-let [winner (game-over? b)]
         winner)))

(fact "one-game : black wins directly"
  (one-game) => :b
  (provided
    (new-board-at-startup) => :board
    (game-over? :board)    => :b))

(defn main-loop
  [] (do 
       (io-show-welcome)
       (loop []
         (io-show-winner (one-game (io-choose-player)))
         (when (io-play-again?) (recur)))
       (io-show-goodbye)))

(fact "main-loop: one game and exit"
  (main-loop) => falsey
  (provided
    (io-show-welcome)   => nil
    (io-choose-player)  => :b
    (one-game :b)       => :w
    (io-show-winner :w) => nil
    (io-play-again?)    => false
    (io-show-goodbye)   => nil))

(println "--------- END OF CHECKERS.CORE ----------" (java.util.Date.))
