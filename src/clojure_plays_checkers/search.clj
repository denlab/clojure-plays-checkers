(ns ^{:doc "Search the game tree"}
  clojure-plays-checkers.search
  (:use     [midje.sweet])
  (:use     [clojure.pprint :only [pprint]])
  (:use     [clojure.walk   :only [macroexpand-all]])
  (:require [clojure.set                       :as set])
  (:require [clojure-plays-checkers.board-util :as u])
  (:require [clojure-plays-checkers.board      :as b])
  (:import (java.text SimpleDateFormat))
  (:import (java.util Date)))

(defn play-move ""
  ([bd] (play-move bd 2)))

(fact "play-move: board finished"
  )


(println "--------- END OF SEARCH ----------" (java.util.Date.))
