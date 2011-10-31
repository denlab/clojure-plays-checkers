(ns clojure-plays-checkers.board-util
  (:use     [midje.sweet])
  (:use     [clojure.pprint :only [pprint]])
  (:use     [clojure.walk   :only [walk macroexpand-all]])
  (:require [clojure.set     :as set])
  (:import (java.text SimpleDateFormat))
  (:import (java.util Date)))

(defn mat-coords "Return all the coords of a matrix of the given size"
  [s] (vec (take (* s s)
                 (iterate (fn [[y x]] (if (= (inc x) s)
                                       [(inc y) 0]
                                       [y (inc x)]))
                          [0 0]))))

(fact
 (mat-coords 3) => [[0 0] [0 1] [0 2]
                    [1 0] [1 1] [1 2]
                    [2 0] [2 1] [2 2]])

(fact
  (mat-coords 3)         => vector?
  (get (mat-coords 3) 0) => vector?)

(defn new-board-mat
  [board] (let [sym->vals {'x :b 'K :b-king 'o :w '0 :w-king}
                s (int (Math/sqrt (count board)))]
            (vec (map vec (partition s (map #(sym->vals %)
                                            board))))))

(fact
 (new-board-mat '[x K .
                  . . .
                  o 0 .]) => [[:b  :b-king nil]
                              [nil nil     nil]
                              [:w  :w-king nil]])

(fact "board is a vec"
  (new-board-mat '[.])         => vector?
  (first (new-board-mat '[.])) => vector?)

(defn new-board-fn "Construct a full board with a player and a matrix."
  [[player & board]]
  (let [s (int (Math/sqrt (count board)))]
    {:size   s
     :player player
     :coords (mat-coords s)
     :board  (new-board-mat board)}))

(fact
 (new-board-fn
  '[:b
    . .
    . .])
 => {:size 2
     :player :b
     :coords [[0 0]]
     :board [[:board]]}
 (provided
  (mat-coords 2) => [[0 0]]
  (new-board-mat '[. .
                   . .]) => [[:board]]))

(defmacro new-board
  [& body] (new-board-fn body))

(fact "Check the macro for building board"
             (new-board :a
                        . .
                        . .)
             => (new-board-fn '[:a
                                . .
                                . .]))

(defn next-player
  [player] (case player
             :b :w
             :w :b))

(fact
  (next-player :b) => :w
  (next-player :w) => :b)

(defn rm-cell
  [board [y x]]
  {:pre [(get-in board [:board y x])]}
  (update-in board [:board y x] (fn [_] nil)))

(fact
 (rm-cell (new-board :x
                     o o
                     o o) [1 0]) => (new-board :x
                                               o o
                                               . o)
 (rm-cell (new-board :x
                     .) [0 0]) => (throws AssertionError))

(defn add-cell
  [{:keys [size] :as board-full} [y x] piece]
  {:pre [(not (get-in board-full [:board y x]))]}
  (let [new-p
        (cond
         (and (= :b piece) (= y 0         ))     :b-king
         (and (= :w piece) (= y (dec size)))     :w-king
         :else                                   piece)]
    (update-in board-full [:board y x] (fn [_] new-p))))

(fact "add-cell simple black"
      (add-cell (new-board :b
                           0 0
                           . 0) [1 0] :b)
      => (new-board :b
                    0 0
                    x 0))

(fact "add-cell assertion"
      (add-cell (new-board :b
                           o) [0 0] :b) => (throws AssertionError))

(fact "add-cell simple white"
      (add-cell (new-board :b
                           . K
                           K K) [0 0] :w)
      => (new-board :b
                    o K
                    K K))

(fact "add-cell kingify black"
      (add-cell (new-board :x
                           . 0
                           0 0) [0 0] :b)
      => (new-board :x
                    K 0
                    0 0))

(fact "add-cell kingify white"
      (add-cell (new-board :x
                           K K
                           . K) [1 0] :w) => (new-board :x
                                                        K K
                                                        0 K))

(defn mv-cell
  [{:keys [board] :as board-full} src dst] (-> board-full
                                               (rm-cell src)
                                               (add-cell dst (get-in board src))))

(fact
 (let [board {:board [[:piece]]}]
   (mv-cell board [0 0] :dst) => :bd2
   (provided
    (rm-cell board [0 0])       => :bd1
    (add-cell :bd1 :dst :piece) => :bd2)))

(fact
 (mv-cell (new-board :b
                     . .
                     . .) [0 0] [1 1]) => (throws AssertionError)

 (mv-cell (new-board :b
                     o .
                     . o) [0 0] [1 1]) => (throws AssertionError))

(println "--------- END OF BOARD-UTIL ----------" (java.util.Date.))

