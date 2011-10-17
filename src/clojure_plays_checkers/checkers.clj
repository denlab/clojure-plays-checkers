(ns clojure-plays-checkers.checkers
  (:use     [midje.sweet])
  (:use     [clojure.pprint :only [pprint]])
  (:use     [clojure.walk   :only [macroexpand-all]])
  (:require [clojure.set     :as set])
  (:import (java.text SimpleDateFormat))
  (:import (java.util Date)))

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
 (vector? (mat-coords 3))         => true
 (vector? (get (mat-coords 3) 0)) => true)

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
      (vector? (new-board-mat '[.]))         => true
      (vector? (first (new-board-mat '[.]))) => true)

(defn new-board-fn
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

(defn in-bound-one?
  [n size] (< -1 n size))

(fact
  (in-bound-one? -1 2) => false
  (in-bound-one?  0 2) => true
  (in-bound-one?  0 1) => true
  (in-bound-one?  0 2) => true)

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

(defn neighboors 
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

(fact "neighboors-of-piece itest can't move backward"
  (neighboors-of-piece [0 1] (new-board :b
                                        . x
                                        . .)) => #{})

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

(defn next-player
  [player] (case player
             :b :w
             :w :b))

(fact
  (next-player :b) => :w
  (next-player :w) => :b)

(defn rm-cell
  [board [y x]] (update-in board [:board y x] (fn [_] nil)))

(future-fact "add precomp on rm-cell: check if not null")

(fact
 (rm-cell (new-board :x
                     o o
                     o o) [1 0]) => (new-board :x
                                               o o
                                               . o))

(defn add-cell
  [{:keys [size] :as board-full} [y x] piece]
  (let [new-p
        (cond
         (or (= :b-king piece) (= :w-king piece)) piece
         (and (= :b piece) (= y 0         ))     :b-king
         (and (= :w piece) (= y (dec size)))     :w-king)]
    (update-in board-full [:board y x] (fn [_] new-p))))

(future-fact "precondition on add-cell : cell must be nil")

(fact "add-cell simple"
      (add-cell (new-board :b
                           0 0
                           . 0) [1 0] :b-king)
      => (new-board :b
                    0 0
                    K 0))

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

(future-fact "a precdondition would be good in mv-cell : src must not be nil, dst must be nil")

(fact
 (let [board {:board [[:piece]]}]
   (mv-cell board [0 0] :dst) => :bd2
   (provided
    (rm-cell board [0 0])       => :bd1
    (add-cell :bd1 :dst :piece) => :bd2)))

(fact "mv-cell itest move backwards if king"
      (let [board (new-board :b
                             K .
                             . .)]
        (mv-cell board [0 0] [1 1]))
      => (new-board :b
                    . .
                    . K))

(defn compute-board-simple
  [board src dst] (update-in (mv-cell board src dst)
                             [:player]
                             next-player))

(fact
 (compute-board-simple :bd :src :dst) => {:player :p2, :other-stuff :s}
 (provided
  (mv-cell :bd :src :dst) => {:player :p1 :other-stuff :s}
  (next-player :p1)       => :p2))

(defn moves-of-pos-simple
  [coord {:keys [size player] :as board}]
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

(future-fact "this fact is not at his place")
(fact "moves-of-pos: itest: can't move because of friend"
  (moves-of-pos [1 0] (new-board :b
                                 . x
                                 x .))
  => {})

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
  [next-coord next-coord2 board-mat player]
  (and (=    (get-in board-mat next-coord) (next-player player))
       (nil? (get-in board-mat next-coord2))))

(tabular
 (fact (jumpable? [1 1] [2 2] ?bd-mat :b) => ?expected)
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
  [coord board-mat size player]
  (reduce (fn [m {:keys [next next2]}]
            (if (jumpable? next next2 board-mat player)
              (conj m {:src coord, :dst next2, :remove next})
              m))
          []
          (neighboors-for-jump coord size)))

(fact
  (possible-jumps :coord :bd-mat :size :player1) => [{:src    :coord
                                                      :dst    :coord-n2-b
                                                      :remove :coord-n-b}]
  (provided
    (neighboors-for-jump :coord :size) => [{:next :coord-n-a, :next2 :coord-n2-a}
                                           {:next :coord-n-b, :next2 :coord-n2-b}]
    (jumpable? :coord-n-a :coord-n2-a :bd-mat :player1) => false
    (jumpable? :coord-n-b :coord-n2-b :bd-mat :player1) => true))

(defn jump-cell
  [bd-mat size player {:keys [src dst remove]}]
  (-> bd-mat
      (rm-cell remove)
      (mv-cell size player src dst)))

(fact "jump-cell"
  (jump-cell :bd-mat :size :player {:src :s, :dst :d, :remove :r}) => :bd-mat2
  (provided
    (rm-cell :bd-mat :r)                   => :bd-mat1
    (mv-cell :bd-mat1 :size :player :s :d) => :bd-mat2))

(defn compute-jump
  [jump bd-mat size player]
  (loop [to-visit [[[jump] bd-mat]] acc {}]
    (if (empty? to-visit)
      acc
      (let [[f-jumps f-bd-mat]            (first to-visit)
            {:keys [dst] :as f-last-jump} (last f-jumps)
            next-bd-mat                   (jump-cell f-bd-mat size player f-last-jump)
            next-jumps                    (possible-jumps dst next-bd-mat size player)]
        (if (empty? next-jumps)
          (recur (next to-visit) (conj acc [f-jumps next-bd-mat]))
          (recur (concat (map (fn [j] [(conj f-jumps j) next-bd-mat])
                              next-jumps)
                         (next to-visit)) acc))))))

(future-fact "missing rule: if the jump leads to a king, stop there")

(fact "compute-jump two jumps due to a branch"
  (let [jmp  {:src :s , :dst :d , :remove :r }
        jmp1 {:src :s1, :dst :d1, :remove :r1}
        jmp2 {:src :d1, :dst :d2, :remove :r2}]
    (compute-jump jmp :bd-mat :size :player) => {[jmp jmp1] :bd-mat1,
                                                 [jmp jmp2] :bd-mat2}
    (provided
      (jump-cell :bd-mat :size :player jmp)      => :bd-mat0
      (possible-jumps :d :bd-mat0 :size :player) => [jmp1 jmp2]

      (jump-cell :bd-mat0 :size :player jmp1)     => :bd-mat1
      (possible-jumps :d1 :bd-mat1 :size :player) => []

      (jump-cell :bd-mat0 :size :player jmp2)     => :bd-mat2
      (possible-jumps :d2 :bd-mat2 :size :player) => [])))

(fact "compute-jump one jump of length two"
  (let [jmp1 {:src :s1, :dst :d1, :remove :r1}
        jmp2 {:src :d1, :dst :d2, :remove :r2}]
    (compute-jump jmp1 :bd-mat1 :size :player) => {[jmp1 jmp2] :bd-mat3}
    (provided
      (jump-cell :bd-mat1 :size :player jmp1)     => :bd-mat2
      (possible-jumps :d1 :bd-mat2 :size :player) => [jmp2]
      (jump-cell :bd-mat2 :size :player jmp2)     => :bd-mat3
      (possible-jumps :d2 :bd-mat3 :size :player) => [])))

(fact "compute-jump simple: only one jump"
  (let [jmp {:src :s :dst :d :remove :r}]
    (compute-jump jmp :bd-mat1 :size :player) => {[jmp] :bd-mat2}
    (provided
      (jump-cell :bd-mat1 :size :player jmp)     => :bd-mat2
      (possible-jumps :d :bd-mat2 :size :player) => [])))

(unfinished )

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
  [jumps bd-mat size player]
  (reduce merge
          (map (fn [j] (let [jumps->bds (compute-jump j bd-mat size player)]
                        (zipmap (map jumps-to-path (keys jumps->bds))
                                (vals jumps->bds))))
               jumps)))

(fact "compute-jumps"
  (compute-jumps [:jmp-a :jmp-b] :bd-mat :size :player) => {:path-a  :bd-mat-a
                                                            :path-b1 :bd-mat-b1
                                                            :path-b2 :bd-mat-b2}
  (provided
    (compute-jump :jmp-a :bd-mat :size :player) => {:jumps-a :bd-mat-a}
    (jumps-to-path :jumps-a)                    => :path-a
    
    (compute-jump :jmp-b :bd-mat :size :player) => {:jumps-b1 :bd-mat-b1
                                                    :jumps-b2 :bd-mat-b2}
    (jumps-to-path :jumps-b1)                   => :path-b1
    (jumps-to-path :jumps-b2)                   => :path-b2))

(defn moves-of-pos-complex
  [coord {:keys [board size player]}]
  (compute-jumps (possible-jumps coord board size player)
                 board
                 size
                 player))

(fact
  (let [bd {:board :bd-mat, :size :sz, :player :p1}]
    (moves-of-pos-complex :coord bd) => {:path1 :bd1, :path2 :bd2}
    (provided
      (possible-jumps :coord :bd-mat :sz :p1) => [:j1 :j2]
      (compute-jumps [:j1 :j2] :bd-mat :sz :p1)   => {:path1 :bd1, :path2 :bd2})))

(fact "moves-of-pos-complex: integration test: can't move because of friend"
  (moves-of-pos-complex [1 0] (new-board :b
                                         . x
                                         x .))
  => nil)

(defn moves-of-pos
  [coord board] (merge (moves-of-pos-simple  coord board)
                       (moves-of-pos-complex coord board)))

(fact "moves-of-pos"
      (moves-of-pos :coord :board) => {:path1 :bd1, :path2 :bd2, :path3 :bd3}
      (provided
       (moves-of-pos-simple  :coord :board) => {:path1 :bd1}
       (moves-of-pos-complex :coord :board) => {:path2 :bd2, :path3, :bd3}))

(fact "moves-of-pos: integration test: can't move because of friend"
  (moves-of-pos [1 0] (new-board :b
                                 . x
                                 x .))
  => {})


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

(future-fact "moves: 'integration test' simple"
      (moves (new-board :b
                        . . .
                        . . .
                        . x .))
      => {[[2 1] [1 0]] (new-board :w
                                   . . .
                                   x . .
                                   . . .)
          [[2 1] [1 2]] (new-board :w
                                   . . .
                                   . . x
                                   . . .)})

(fact "moves: itest can't move because of wall"
  (moves (new-board :b
                    x .
                    . .)) => {}
  (moves (new-board :w
                    . .
                    . o)) => {})

(future-fact "moves: itest can move if king"
  (moves (new-board :b
                    K .
                    . .)) => {[[0 0] [1 1]] (new-board :w
                                                       . .
                                                       . K)})

(fact "moves: itest can't move because of friend"
  (moves (new-board :b
                    . x
                    x .)) => {}
  (moves (new-board :w
                    . o
                    o .)) => {})

(fact "moves: itest can't move because of enemy"
  (moves (new-board :b
                    . o
                    x .)) => {}
  (moves (new-board :w
                    . o
                    x .)) => {})



(println "--------- END OF CHECKERS ----------" (java.util.Date.))