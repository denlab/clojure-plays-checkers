(ns ^{:doc "Search the game tree"}
  clojure-plays-checkers.search
  (:use     [midje.sweet])
  (:use     [clojure.pprint :only [pp pprint]])
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

  ;; branch? is a fn that, given a node, returns true if can have
  ;; children, even if it currently doesn't.

  ;; children is a fn that, given a branch node, returns a seq of its
  ;; children.

  ;; make-node is a fn that, given an existing node and a seq of
  ;; children, returns a new branch node with the supplied children.

  ;; root is the root node.

(defn children
  [[_ bd]] (vals (b/moves bd)))

(fact
 (children [:depth :bd]) => [:bd1 :bd2]
 (provided
  (b/moves :bd) => {:path1 :bd1 :path2 :bd2}))

(defn make-node
  [_ child] child)

(fact
 (make-node :nd :child) => :child)

(def *start-bd* (u/new-board :b
                             . o . o .
                             o . o . o
                             . . . . .
                             x . x . x
                             . x . x .))

(def z (z/zipper (b-branch? 3)
                 children
                 make-node
                 [0 *start-bd*]))

(defn play-move ""
  ([bd] (play-move bd 2)))

(future-fact "play-move: board finished"
)

(println "--------- END OF SEARCH ----------" (java.util.Date.))
