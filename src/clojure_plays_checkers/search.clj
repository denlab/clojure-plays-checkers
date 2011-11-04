(ns ^{:doc "Search the game tree"}
  clojure-plays-checkers.search
  (:use     [midje.sweet])
  (:use     [clojure.pprint :only [pprint]])
  (:use     [clojure.walk   :only [macroexpand-all]])
  (:require [clojure.set                       :as set])
  (:require [clojure-plays-checkers.board-util :as u])
  (:require [clojure-plays-checkers.board      :as b])
  (:require [clojure.zip                       :as z])
  (:import (java.text SimpleDateFormat))
  (:import (java.util Date)))

(defn b-branch? "Returns true if the board has possible moves AND max depth not reached"
  [max-depth] (fn [[depth bd]] (and (< depth max-depth)
                                   (not (empty? (b/moves bd))))))

(fact "b-branch?: can move, max depth not reached"
      ((b-branch? 1) [0 :board]) => true
      (provided
       (b/moves :board) => {:path :board}))

(fact "b-branch?: can move, but max depth reached"
      ((b-branch? 1) [1 :board]) => false)

(fact "b-branch?: max depth not reached, but cannot move"
      ((b-branch? 1) [0 :board]) => false
      (provided
       (b/moves :board) => {}))

(defn play-move ""
  ([bd] (play-move bd 2)))

(fact "play-move: board finished"
)

(println "--------- END OF SEARCH ----------" (java.util.Date.))
