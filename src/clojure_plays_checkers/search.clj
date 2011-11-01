(ns ^{:doc "Search the game tree"}
  clojure-plays-checkers.search
  (:use     [midje.sweet])
  (:use     [clojure.pprint :only [pprint]])
  (:use     [clojure.walk   :only [macroexpand-all]])
  (:require [clojure.set                       :as set])
  (:require [clojure-plays-checkers.board-util :as u])
  (:import (java.text SimpleDateFormat))
  (:import (java.util Date)))

(defn play-move
  [bd])

(fact "play-move"
  )
