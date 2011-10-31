(ns ^{:doc "Try to find a good solution to play."}
  clojure-plays-checkers.find
  (:use     [midje.sweet])
  (:use     [clojure.pprint :only [pprint]])
  (:use     [clojure.walk   :only [macroexpand-all]])
  (:require [clojure.set                       :as set])
  (:require [clojure-plays-checkers.board-util :as u])
  (:import (java.text SimpleDateFormat))
  (:import (java.util Date)))

(defn play-move
  [bd])

(future-fact "play-move")
