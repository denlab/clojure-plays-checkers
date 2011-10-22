(ns clojure-plays-checkers.core
  (:use     [midje.sweet])
  (:use     [clojure.pprint :only [pprint]])
  (:use     [clojure.walk   :only [macroexpand-all]])
  (:require [clojure.set     :as set])
  (:import (java.text SimpleDateFormat))
  (:import (java.util Date)))

(unfinished io-play-again? io-show-winner io-choose-player
            io-show-welcome game-over?  new-board-at-startup )

(defn sub-loop "Loop which will organise one game"
  [] (let [b (new-board-at-startup)]
       (if-let [winner (game-over? b)]
         winner)))

(fact "sub-loop : black wins directly"
  (sub-loop) => :b
  (provided
    (new-board-at-startup) => :board
    (game-over? :board) => :b))

(defn main-loop
  []
  (io-show-welcome)
  (loop [cnt 0]
    (let [player (io-choose-player)
          winner (sub-loop player)]
      (io-show-winner winner)
      (if (> cnt 4)
        :overflow
        (if (io-play-again?)
          (recur (inc cnt)))))))

(fact "main-loop: one party and exit"
  (main-loop) => falsey
  (provided
    (io-show-welcome)   => nil
    (io-choose-player)  => :b
    (sub-loop :b)       => :w
    (io-show-winner :w) => nil
    (io-play-again?)    => false))

(fact "main-loop: 2 parties and exit"
  (main-loop) => falsey
  (provided
    (io-show-welcome)   => nil
    (io-choose-player)  => :b
    (sub-loop :b)       => :w
    (io-show-winner :w) => nil
    (io-play-again?)    => true
    (io-choose-player)  => :b
    (sub-loop :b)       => :w
    (io-show-winner :w) => nil
    (io-play-again?)    => false))

()

(println "--------- END OF CHECKERS.CORE ----------" (java.util.Date.))
