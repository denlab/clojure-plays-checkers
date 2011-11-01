(ns clojure-plays-checkers.core
  (:use     [midje.sweet])
  (:use     [clojure.pprint :only [pprint]])
  (:use     [clojure.walk   :only [macroexpand-all]])
  (:require [clojure.set                       :as set])
  (:require [clojure-plays-checkers.board-util :as u])
  (:require [clojure-plays-checkers.find       :as s])
  (:import (java.text SimpleDateFormat))
  (:import (java.util Date)))

(unfinished io-play-move io-show-goodbye io-play-again?
            io-show-winner io-choose-player io-show-welcome game-over?
            new-board-at-startup )

(defn one-game "Plays one game, the human player is given in param :b | :w"
  [human] (let [b        (new-board-at-startup)
                human->fn {:b io-play-move :w s/play-move}
                black-fn  (human human->fn)
                white-fn  ((u/next-player human) human->fn)]
            (loop [curr-bd b
                   curr-fn black-fn
                   next-fn (interleave (repeat white-fn) (repeat black-fn))]
              (let [bd-n   (curr-fn curr-bd)
                    winner (game-over? bd-n)] (if winner
                                                winner
                                                (recur bd-n (first next-fn) (next next-fn)))))))

(fact "one-game : human white, computer plays, human plays and win"
  (one-game :w) => :w
  (provided
    (new-board-at-startup) => :bd
    (s/play-move    :bd)     => :bd1
    (game-over?   :bd1)    => false
    (io-play-move :bd1)    => :bd2
    (game-over?   :bd2)    => :w))

(fact "one-game : human black, human plays, computer plays, human play and win"
  (one-game :b) => :b
  (provided
    (new-board-at-startup) => :bd
    (io-play-move :bd)  => :bd1
    (game-over?   :bd1) => false
    (s/play-move    :bd1)  => :bd2
    (game-over?   :bd2) => false
    (io-play-move :bd2) => :bd3
    (game-over?   :bd3) => :b))

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

(println "--------- END OF CORE ----------" (java.util.Date.))
