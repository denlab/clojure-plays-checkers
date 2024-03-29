(ns clojure-plays-checkers.board
  (:use     [midje.sweet])
  (:use     [clojure.pprint :only [pprint]])
  (:use     [clojure.walk   :only [walk macroexpand-all]])
  (:require [clojure.set                       :as set])
  (:require [clojure-plays-checkers.board-util :as u])
  (:import (java.text SimpleDateFormat))
  (:import (java.util Date)))

(unfinished game-over-draw? game-over-cannot-move?
            game-over-all-captured?)

(defn print-sym-fn-
  [body]
  (list 'println
        (concat ['str (str (first body) "=") (first body)]
                (mapcat #(list (str ",\t" % "=") %)
                        (next body)))))

(fact
  (print-sym-fn- ['a 'b 'c]) => '(println (str "a=" a ",\tb=" b ",\tc=" c)))

(defmacro
  print-sym [& body]
  (print-sym-fn- body))

(fact
  (let [a 1 b 2]
    (with-out-str (print-sym a b))) => "a=1,\tb=2\n")


(defn in-bound-one?
  [n size] (< -1 n size))

(fact
  (in-bound-one? -1  2) => false
  (in-bound-one?  0 -1) => false
  (in-bound-one?  0  2) => true
  (in-bound-one?  0  1) => true
  (in-bound-one?  0  2) => true)

(defn in-bound?
  [coord size] (every? #(in-bound-one? % size)
                       coord))

(fact
  (in-bound? [:y :x] :size) => true
  (provided (in-bound-one? :y :size) => true
            (in-bound-one? :x :size) => true))

(fact
  (in-bound? [:y :x] :size) => false
  (provided (in-bound-one? :y :size) => false))

(fact
  (in-bound? [:y :x] :size) => false
  (provided (in-bound-one? :y :size) => true
            (in-bound-one? :x :size) => false))

(defn neighboors "Compute the coordinates of the neighboors from a place with coordinates [y x]"
  ([size [y x]]
     (set (filter (fn [coord] (in-bound? coord size))
                  [[(dec y) (dec x)] [(dec y) (inc x)]
                   [(inc y) (dec x)] [(inc y) (inc x)]]))))

(fact
  (neighboors 3 [1 1]) => #{[0 0] [0 2] [2 0] [2 2]}
  (provided
    (in-bound? [0 0] 3) => true
    (in-bound? [0 2] 3) => true
    (in-bound? [2 0] 3) => true
    (in-bound? [2 2] 3) => true))

(defn neighboors-of-piece
  [[y x] {:keys [size board]}]
  (let [piece (get-in board [y x])
        filter-fun (piece {:b < :w > :b-king not= :w-king not=})]
    (set (filter (fn [[ly lx]] (filter-fun ly y))
                 (neighboors size [y x])))))

(fact "neighboors-of-piece white"
  (neighboors-of-piece [0 0] {:size :sz :board [[:w]]}) => #{[2 2]}
  (provided
    (neighboors :sz [0 0]) => #{[0 0] [2 2]}))

(fact "neighboors-of-piece black"
  (neighboors-of-piece [1 1] {:size :sz :board [[nil nil]
                                                [nil :b]]}) => #{[0 0]}
  (provided
    (neighboors :sz [1 1]) => #{[0 0] [2 2]}))

(fact "neighboors-of-piece black king"
  (neighboors-of-piece [1 1] {:size :sz :board [[nil nil]
                                                [nil :b-king]]}) => #{[0 0] [2 2]}
  (provided
    (neighboors :sz [1 1]) => #{[0 0] [2 2]}))

(fact "neighboors-of-piece white king"
  (neighboors-of-piece [1 1] {:size :sz :board [[nil nil]
                                                [nil :w-king]]}) => #{[0 0] [2 2]}
  (provided
    (neighboors :sz [1 1]) => #{[0 0] [2 2]}))

(defn empty-neighboors-of-piece
  [coord {:keys [board] :as bd-full}]
  (filter #(nil? (get-in board %))
          (neighboors-of-piece coord bd-full))) 

(fact "empty-neighboors-of-piece"
  (let [board {:board [[:x :x ]
                       [:x nil]]}]
    (empty-neighboors-of-piece :coord board) => [[1 1]]
    (provided
      (neighboors-of-piece :coord board) => [[0 0] [0 1]
                                             [1 0] [1 1]])))

(defn compute-board-simple
  [board src dst] (u/mv-cell board src dst))

(fact "compute-board-simple"
  (compute-board-simple :bd :src :dst) => :bd2
  (provided
    (u/mv-cell :bd :src :dst) => :bd2))

(defn moves-of-pos-simple
  [coord board]
  (reduce
   (fn [m v] (if-let [next-bd (compute-board-simple board coord v)]
              (conj m [[coord v] next-bd])
              m))
   {}
   (empty-neighboors-of-piece coord board)))

(fact "moves-of-pos-simple"
  (let [bd {:player :b :size 3}]
    (moves-of-pos-simple :co bd) => {[:co :co1] :bd2, [:co :co2] :bd3}
    (provided
      (empty-neighboors-of-piece :co bd) => #{:co1 :co2 :co3}
      (compute-board-simple bd :co :co1) => :bd2
      (compute-board-simple bd :co :co2) => :bd3
      (compute-board-simple bd :co :co3) => nil)))

(defn neighboors-for-jump-may-be-out-of-bound
  [[y x] size] (let [y- (dec y) y+ (inc y) x- (dec x) x+ (inc x)]
                 [{:next [y- x-] :next2 [(dec y-) (dec x-)]}
                  {:next [y- x+] :next2 [(dec y-) (inc x+)]}
                  {:next [y+ x+] :next2 [(inc y+) (inc x+)]}
                  {:next [y+ x-] :next2 [(inc y+) (dec x-)]}]))

(fact
  (neighboors-for-jump-may-be-out-of-bound [2 2] 5) => [{:next [1 1] :next2 [0 0]}
                                                        {:next [1 3] :next2 [0 4]}
                                                        {:next [3 3] :next2 [4 4]}
                                                        {:next [3 1] :next2 [4 0]}])

(defn neighboors-for-jump
  [coord size] (filter (fn [{:keys [next next2]}] (and (in-bound? next  size)
                                                      (in-bound? next2 size)))
                       (neighboors-for-jump-may-be-out-of-bound coord size)))

(fact
  (neighboors-for-jump :coord :size) => [{:next :cn3 :next2 :cn4}]
  (provided
    (neighboors-for-jump-may-be-out-of-bound :coord :size) => [{:next :cn1 :next2 :cn2}
                                                               {:next :cn3 :next2 :cn4}]
    (in-bound? :cn1 :size) => true
    (in-bound? :cn2 :size) => false
    (in-bound? :cn3 :size) => true
    (in-bound? :cn4 :size) => true))

(defn jumpable?
  [next-coord next-coord2 {:keys [board player]}]
  (and (=    (get-in board next-coord) (u/next-player player))
       (nil? (get-in board next-coord2))))

(tabular
 (fact (jumpable? [1 1] [2 2] {:board ?bd-mat :player :b}) => ?expected)
 ?bd-mat         ?expected

 [[:b  nil nil]
  [nil :w  nil]
  [nil nil nil]] true

  [[:b  nil nil]
  [nil :x  nil]
  [nil nil nil]] false

  [[:b  nil nil]
  [nil :w  nil]
  [nil nil :x]] false)

(defn possible-jumps
  [coord bd]
  (reduce (fn [m {:keys [next next2]}]
            (if (jumpable? next next2 bd)
              (conj m {:src coord, :dst next2, :remove next})
              m))
          []
          (neighboors-for-jump coord (:size bd))))

(fact
  (let [bd {:size :s}]
    (possible-jumps :coord bd) => [{:src    :coord
                                    :dst    :coord-n2-b
                                    :remove :coord-n-b}]
    (provided
      (neighboors-for-jump :coord :s) => [{:next :coord-n-a, :next2 :coord-n2-a}
                                          {:next :coord-n-b, :next2 :coord-n2-b}]
      (jumpable? :coord-n-a :coord-n2-a bd) => false
      (jumpable? :coord-n-b :coord-n2-b bd) => true)))

(defn jump-cell
  [bd {:keys [src dst remove]}]
  (-> bd
      (u/rm-cell remove)
      (u/mv-cell src dst)))

(fact "jump-cell"
      (jump-cell :bd {:src :s, :dst :d, :remove :r}) => :bd2
      (provided
       (u/rm-cell :bd  :r)    => :bd1
       (u/mv-cell :bd1 :s :d) => :bd2))

(defn king?
  [piece]  (or (= piece :b-king)
               (= piece :w-king)))

(fact "king?"
      (king? :b) => false
      (king? :w) => false
      (king? :b-king) => true
      (king? :w-king) => true)

(defn king-at-pos?
  [bd-mat pos] (king? (get-in bd-mat pos)))

(fact "king-at-pos?"
      (king-at-pos? [[:piece]] [0 0]) => :result
      (provided (king? :piece) => :result))

(defn compute-jump
  [jump bd]
  (loop [to-visit [[[jump] bd]] acc {}]
    (if (empty? to-visit)
      acc
      (let [[f-jumps f-bd]                   (first to-visit)
            {:keys [dst] :as f-last-jump}    (last f-jumps)
            {next-bd-mat :board :as next-bd} (jump-cell f-bd f-last-jump)
            next-jumps                       (possible-jumps dst next-bd)]
        (if (or (king-at-pos? next-bd-mat dst) (empty? next-jumps))
          (recur (next to-visit)
                 (conj acc [f-jumps next-bd]))
          (recur (concat (map (fn [j] [(conj f-jumps j) next-bd])
                              next-jumps)
                         (next to-visit)) acc))))))

(fact "compute-jump if the jump leads to a kingification, stop there"
      (let [jmp     {:dst [0 0]}
            bd-mat1 [[:piece]]]
        (compute-jump jmp {:board :bd-mat :size :size :player :player}) => {[jmp] {:board bd-mat1}}
        (provided
          (jump-cell {:board :bd-mat :size :size :player :player} jmp)        => {:board bd-mat1}
          (possible-jumps [0 0] {:board bd-mat1}) => [:jmp1 :jmp2]
         (king-at-pos? bd-mat1 [0 0])                               => true)))

(fact "compute-jump two jumps due to a branch"
  (let [jmp  {:src :s , :dst :d , :remove :r }
        jmp1 {:src :s1, :dst :d1, :remove :r1}
        jmp2 {:src :d1, :dst :d2, :remove :r2}]
    (compute-jump jmp {:board :bd-mat :size :size :player :player}) => {[jmp jmp1] {:board :bd-mat1}
                                                                        [jmp jmp2] {:board :bd-mat2}}
    (provided
      (jump-cell {:board :bd-mat :size :size :player :player} jmp)      => {:board :bd-mat0}
      (king-at-pos? :bd-mat0 :d) => false
      (possible-jumps :d {:board :bd-mat0}) => [jmp1 jmp2]

      (jump-cell {:board :bd-mat0} jmp1)     => {:board :bd-mat1}
      (king-at-pos? :bd-mat1 :d1) => false
      (possible-jumps :d1 {:board :bd-mat1}) => []

      (jump-cell {:board :bd-mat0} jmp2)     => {:board :bd-mat2}
      (king-at-pos? :bd-mat2 :d2) => false
      (possible-jumps :d2 {:board :bd-mat2}) => [])))

(fact "compute-jump one jump of length two"
  (let [jmp1 {:src :s1, :dst :d1, :remove :r1}
        jmp2 {:src :d1, :dst :d2, :remove :r2}]
    (compute-jump jmp1 {:board :bd-mat1 :size :size :player :player}) => {[jmp1 jmp2] {:board :bd-mat3}}
    (provided
      (jump-cell {:board :bd-mat1 :size :size :player :player} jmp1)     => {:board :bd-mat2}
      (king-at-pos? :bd-mat2 :d1) => false
      (possible-jumps :d1 {:board :bd-mat2}) => [jmp2]

      (jump-cell {:board :bd-mat2} jmp2)     => {:board :bd-mat3}
      (king-at-pos? :bd-mat3 :d2) => false
      (possible-jumps :d2 {:board :bd-mat3}) => [])))

(fact "compute-jump simple: only one jump"
  (let [jmp {:src :s :dst :d :remove :r}]
    (compute-jump jmp {:board :bd-mat1 :size :size :player :player}) => {[jmp] {:board :bd-mat2}}
    (provided
      (jump-cell {:board :bd-mat1 :size :size :player :player} jmp) => {:board :bd-mat2}
      (king-at-pos? :bd-mat2 :d)                                    => false
      (possible-jumps :d {:board :bd-mat2})                         => [])))

(defn jumps-to-path
  [jumps] (conj
           (reduce (fn [r {:keys [src]}] (conj r src))
                   []
                   jumps)
           (:dst (last jumps))))

(fact
  (jumps-to-path
   [{:src :s1 :dst :d1 :remove :r1}]) => [:s1 :d1]
  (jumps-to-path
   [{:src :s1 :dst :d1 :remove :r1}
    {:src :d1 :dst :d2 :remove :r2}
    {:src :d2 :dst :d3 :remove :r3}]) => [:s1 :d1 :d2 :d3])

(defn compute-jumps
  [jumps bd]
  (reduce merge
          (map (fn [j] (walk (fn [[j b]] [(jumps-to-path j) b])
                            identity
                            (compute-jump j bd)))
               jumps)))

(fact "compute-jumps"
  (compute-jumps [:jmp-a :jmp-b] :bd)
  => {:path-a  :bd-a
      :path-b1 :bd-b1
      :path-b2 :bd-b2}
  (provided
    (compute-jump :jmp-a :bd) => {:jumps-a :bd-a}
    (jumps-to-path :jumps-a)  => :path-a
    
    (compute-jump :jmp-b :bd) => {:jumps-b1 :bd-b1, :jumps-b2 :bd-b2}
    (jumps-to-path :jumps-b1) => :path-b1
    (jumps-to-path :jumps-b2) => :path-b2))

(defn set-board-next-player
  [b] (update-in b [:player] u/next-player))

(fact (set-board-next-player {:player :p1}) => {:player :p2}
  (provided (u/next-player :p1) => :p2))

(defn moves-of-pos-complex
  [coord bd] (compute-jumps (possible-jumps coord bd)
                            bd))
(fact "moves-of-pos-complex"
  (moves-of-pos-complex :coord :bd) => {:path1 :bd1, :path2 :bd2}
  (provided
    (possible-jumps :coord :bd)   => [:j1 :j2]
    (compute-jumps [:j1 :j2] :bd) => {:path1 :bd1, :path2 :bd2}))

(defn moves-of-pos
  [coord board] (walk (fn [[p b]] [p (set-board-next-player b)])
                      identity
                      (merge (moves-of-pos-simple  coord board)
                             (moves-of-pos-complex coord board))))

(fact "moves-of-pos"
      (moves-of-pos :coord :board) => {:path1 :bd1n, :path2 :bd2n, :path3 :bd3n}
      (provided
       (moves-of-pos-simple  :coord :board) => {:path1 :bd1}
       (moves-of-pos-complex :coord :board) => {:path2 :bd2, :path3, :bd3}
       (set-board-next-player :bd1)         => :bd1n
       (set-board-next-player :bd2)         => :bd2n
       (set-board-next-player :bd3)         => :bd3n))

(defn piece-of-player?
  [piece player] (case player
                   :b (or (= :b piece) (= :b-king piece))
                   :w (or (= :w piece) (= :w-king piece)))) 

(tabular 
 (fact (piece-of-player? ?piece ?player) => ?expected)
 ?piece ?player ?expected
 nil     :b     false
 :b      :b     true
 :b-king :b     true
 :w      :b     false
 :w-king :b     false
 nil     :w     false
 :b      :w     false
 :b-king :w     false
 :w      :w     true
 :w-king :w     true)

(defn coord-of-player?
  [coord {:keys [player board]}] (piece-of-player? (get-in board coord)
                                                   player))

(fact "coord-of-player?"
  (let [board {:player :p :board [[:piece]]}]
    (coord-of-player? [0 0] board) => :result
    (provided
      (piece-of-player? :piece :p) => :result)))

(defn coords-of-player
  [{:keys [coords] :as board}] (filter #(coord-of-player? % board)
                                       coords))

(fact
  (let [board {:coords [:c1 :c2 :c3]}]
    (coords-of-player board) => [:c1 :c3]
    (provided (coord-of-player? :c1 board) => true
              (coord-of-player? :c2 board) => false
              (coord-of-player? :c3 board) => true)))

(defn moves
  [board] (reduce (fn [m coord] (merge m (moves-of-pos coord board)))
                  {} (coords-of-player board)))

(fact
  (moves :board) => {:path1 :bd1 :path2 :bd2}
  (provided
    (coords-of-player :board) => [:c1 :c2]
    (moves-of-pos :c1 :board) => {:path1 :bd1}
    (moves-of-pos :c2 :board) => {:path2 :bd2}))

(fact "moves: itest simple mv"
      (moves (u/new-board :b
                        . . 
                        . x))
      => {[[1 1] [0 0]] (u/new-board :w
                                   K .
                                   . .)})

(fact "moves: itest simple mv on 3x3"
      (moves (u/new-board :b
                        . . .
                        . . .
                        . . x))
      => {[[2 2] [1 1]] (u/new-board :w
                                   . . .
                                   . x .
                                   . . .)})

(fact "moves: itest simple mv, left or right"
      (moves (u/new-board :b
                        . . .
                        . . .
                        . x .))
      => {[[2 1] [1 0]] (u/new-board :w
                                   . . .
                                   x . .
                                   . . .)
          [[2 1] [1 2]] (u/new-board :w
                                   . . .
                                   . . x
                                   . . .)})

(fact "moves: itest can't move because of wall"
  (moves (u/new-board :b
                    x .
                    . .)) => {}
  (moves (u/new-board :w
                    . .
                    . o)) => {})

(fact "moves: itest can move backwards if king"
  (moves (u/new-board :b
                    K .
                    . .)) => {[[0 0] [1 1]] (u/new-board :w
                                                       . .
                                                       . K)})

(fact "moves: itest can't move because of friend"
  (moves (u/new-board :b
                    . x
                    x .)) => {}
  (moves (u/new-board :w
                    . o
                    o .)) => {})

(fact "moves: itest can't move because of enemy"
  (moves (u/new-board :b
                    . o
                    x .)) => {}
  (moves (u/new-board :w
                    . o
                    x .)) => {})

(fact "moves: itest jump simple"
  (moves (u/new-board :b
                    . . .
                    . o .
                    . . x)) 
  => {[[2 2] [0 0]]
      (u/new-board :w
                 K . .
                 . . .
                 . . .)})

(fact "moves: itest jump of length 2"
  (moves (u/new-board :b
                    . . . . .
                    . . . . .
                    . . . . .
                    . o . o .
                    . . . . x)) 
  => {[[4 4] [2 2] [4 0]]
      (u/new-board :w
                    . . . . .
                    . . . . .
                    . . . . .
                    . . . . .
                    x . . . .)})

(fact "moves: itest jump with 2 possibilities"
  (moves (u/new-board :b
                    . . . . .
                    . . . o .
                    . . . . .
                    . o . o .
                    . . . . x)) 
  => {[[4 4] [2 2] [4 0]]
      (u/new-board :w
                 . . . . .
                 . . . o .
                 . . . . .
                 . . . . .
                 x . . . .)
      [[4 4] [2 2] [0 4]]
      (u/new-board :w
                 . . . . K
                 . . . . .
                 . . . . .
                 . o . . .
                 . . . . .)})

(fact "moves: itest can't jump because there's 2 enemies"
  (moves (u/new-board :b
                    . . . . .
                    . . . . .
                    . . o . .
                    . . . o .
                    . . . . x)) 
  => {})

(fact "moves: itest combo jump stopped by a kingification"
  (moves (u/new-board :b
                    . . . . .
                    . o . o .
                    x . . . .
                    . . . . .
                    . . . . .)) 
  => {[[2 0] [0 2]] (u/new-board :w
                               . . K . .
                               . . . o .
                               . . . . .
                               . . . . .
                               . . . . .)})



(defn game-over?
  [bd] (or (game-over-all-captured? bd)
           (game-over-cannot-move?  bd)
           (game-over-draw?         bd)))

(fact "game-over?: all captured"
      (game-over? :bd) => :some-val
      (provided
       (game-over-all-captured? :bd) => :some-val))

(fact "game-over?: can't move"
      (game-over? :bd) => :some-val
      (provided
       (game-over-all-captured? :bd) => nil
       (game-over-cannot-move?  :bd) => :some-val))

(fact "game-over?: not yet"
      (game-over? :bd) => nil
      (provided
       (game-over-all-captured? :bd) => nil
       (game-over-cannot-move?  :bd) => nil
       (game-over-draw?         :bd) => nil))

(fact "game-over?: draw"
      (game-over? :bd) => :draw
      (provided
       (game-over-all-captured? :bd) => nil
       (game-over-cannot-move?  :bd) => nil
       (game-over-draw?         :bd) => :draw))

(println "--------- END OF BOARD ----------" (java.util.Date.))
