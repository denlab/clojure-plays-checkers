(ns clojure-plays-checkers.checkers
  (:use     [midje.sweet])
  (:use     [clojure.pprint :only [pprint]])
  (:use     [clojure.walk   :only [macroexpand-all]])
  (:require [clojure.set     :as set])
  (:import (java.text SimpleDateFormat))
  (:import (java.util Date)))

(unfinished )

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

(defn compute-board-simple
  [board src dst] (update-in (mv-cell board src dst)
                             [:player]
                             next-player))

(fact
 (compute-board-simple :bd :src :dst) => {:player :p2, :other-stuff :s}
 (provided
  (mv-cell :bd :src :dst) => {:player :p1 :other-stuff :s}
  (next-player :p1)       => :p2))

(fact "compute-board-simple: itest simple mv on 3x3, todelete when ok"
      (compute-board-simple (new-board :b
                                       . . .
                                       . . .
                                       . . x)
                            [2 2] [1 1])
      => (new-board :w
                     . . .
                     . x .
                     . . .))

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

(fact "moves-of-pos-simple: itest simple mv on 3x3, todelete when ok"
             (moves-of-pos-simple [2 2] (new-board :b
                                                   . . .
                                                   . . .
                                                   . . x))
             => {[[2 2] [1 1]] (new-board :w
                                          . . .
                                          . x .
                                          . . .)})

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

(defn jumpable2?
  [next-coord next-coord2 {:keys [board player]}]
  (and (=    (get-in board next-coord) (next-player player))
       (nil? (get-in board next-coord2))))

(tabular
 (fact (jumpable2? [1 1] [2 2] {:board ?bd-mat :player :b}) => ?expected)
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

(future-fact "refactoring in progress: plug jumpable2 when other are ready")

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

(defn possible-jumps2
  [coord bd]
  (reduce (fn [m {:keys [next next2]}]
            (if (jumpable2? next next2 bd)
              (conj m {:src coord, :dst next2, :remove next})
              m))
          []
          (neighboors-for-jump coord (:size bd))))

(future-fact "test below is simplifiable:)")
(fact
  (possible-jumps2 :coord {:board :bd-mat :size :size :player :player1}) => [{:src    :coord
                                                                              :dst    :coord-n2-b
                                                                              :remove :coord-n-b}]
  (provided
    (neighboors-for-jump :coord :size) => [{:next :coord-n-a, :next2 :coord-n2-a}
                                           {:next :coord-n-b, :next2 :coord-n2-b}]
    (jumpable2? :coord-n-a :coord-n2-a {:board :bd-mat :size :size :player :player1}) => false
    (jumpable2? :coord-n-b :coord-n2-b {:board :bd-mat :size :size :player :player1}) => true))

(defn jump-cell
  [bd {:keys [src dst remove]}]
  (-> bd
      (rm-cell remove)
      (mv-cell src dst)))

(fact "jump-cell"
      (jump-cell :bd {:src :s, :dst :d, :remove :r}) => :bd2
      (provided
       (rm-cell :bd  :r)    => :bd1
       (mv-cell :bd1 :s :d) => :bd2))

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
  [jump {:keys [board size player] :as bd-full}]
  (let [bd-mat board]
    (loop [to-visit [[[jump] bd-full]] acc {}]
      (if (empty? to-visit)
        acc
        (let [[f-jumps f-bd]            (first to-visit)
              {:keys [dst] :as f-last-jump} (last f-jumps)
              {next-bd-mat :board :as next-bd} (jump-cell f-bd f-last-jump)
              next-jumps                    (possible-jumps dst next-bd-mat size player)]
          (if (or (king-at-pos? next-bd-mat dst) (empty? next-jumps))
            (recur (next to-visit) (conj acc [f-jumps next-bd]))
            (recur (concat (map (fn [j] [(conj f-jumps j) next-bd])
                                next-jumps)
                           (next to-visit)) acc)))))))

(fact "compute-jump if the jump leads to a kingification, stop there"
      (let [jmp     {:dst [0 0]}
            bd-mat1 [[:piece]]]
        (compute-jump jmp {:board :bd-mat :size :size :player :player}) => {[jmp] {:board bd-mat1}}
        (provided
          (jump-cell {:board :bd-mat :size :size :player :player} jmp)        => {:board bd-mat1}
         (possible-jumps [0 0] bd-mat1 :size :player) => [:jmp1 :jmp2]
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
      (possible-jumps :d :bd-mat0 :size :player) => [jmp1 jmp2]

      (jump-cell {:board :bd-mat0} jmp1)     => {:board :bd-mat1}
      (king-at-pos? :bd-mat1 :d1) => false
      (possible-jumps :d1 :bd-mat1 :size :player) => []

      (jump-cell {:board :bd-mat0} jmp2)     => {:board :bd-mat2}
      (king-at-pos? :bd-mat2 :d2) => false
      (possible-jumps :d2 :bd-mat2 :size :player) => [])))

(fact "compute-jump one jump of length two"
  (let [jmp1 {:src :s1, :dst :d1, :remove :r1}
        jmp2 {:src :d1, :dst :d2, :remove :r2}]
    (compute-jump jmp1 {:board :bd-mat1 :size :size :player :player}) => {[jmp1 jmp2] {:board :bd-mat3}}
    (provided
      (jump-cell {:board :bd-mat1 :size :size :player :player} jmp1)     => {:board :bd-mat2}
      (king-at-pos? :bd-mat2 :d1) => false
      (possible-jumps :d1 :bd-mat2 :size :player) => [jmp2]

      (jump-cell {:board :bd-mat2} jmp2)     => {:board :bd-mat3}
      (king-at-pos? :bd-mat3 :d2) => false
      (possible-jumps :d2 :bd-mat3 :size :player) => [])))

(fact "compute-jump simple: only one jump"
  (let [jmp {:src :s :dst :d :remove :r}]
    (compute-jump jmp {:board :bd-mat1 :size :size :player :player}) => {[jmp] {:board :bd-mat2}}
    (provided
      (jump-cell {:board :bd-mat1 :size :size :player :player} jmp)     => {:board :bd-mat2}
      (king-at-pos? :bd-mat2 :d)                 => false
      (possible-jumps :d :bd-mat2 :size :player) => [])))

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
  [jumps {:keys [board size player] :as full-board}]
  (reduce merge
          (map (fn [j] (let [jumps->bds (compute-jump j full-board)]
                        (zipmap (map jumps-to-path (keys jumps->bds))
                                (vals jumps->bds))))
               jumps)))

(fact "compute-jumps"
      (compute-jumps [:jmp-a :jmp-b] {:board :bd-mat, :size :size, :player :player})
      => {:path-a  :bd-a
          :path-b1 :bd-b1
          :path-b2 :bd-b2}
      (provided
       (compute-jump :jmp-a {:board :bd-mat :size :size :player :player}) => {:jumps-a :bd-a}
       (jumps-to-path :jumps-a)                    => :path-a
    
       (compute-jump :jmp-b {:board :bd-mat :size :size :player :player}) => {:jumps-b1 :bd-b1
                                                                               :jumps-b2 :bd-b2}
       (jumps-to-path :jumps-b1)                   => :path-b1
       (jumps-to-path :jumps-b2)                   => :path-b2))


(defn moves-of-pos-complex
  [coord bd] (compute-jumps (possible-jumps2 coord bd)
                            bd))

(future-fact "fix here: the player is not switched")

(fact
  (moves-of-pos-complex :coord :bd) => {:path1 :bd1, :path2 :bd2}
  (provided
    (possible-jumps2 :coord :bd)  => [:j1 :j2]
    (compute-jumps [:j1 :j2] :bd) => {:path1 :bd1, :path2 :bd2}))

(defn moves-of-pos
  [coord board] (merge (moves-of-pos-simple  coord board)
                       (moves-of-pos-complex coord board)))

(fact "moves-of-pos"
      (moves-of-pos :coord :board) => {:path1 :bd1, :path2 :bd2, :path3 :bd3}
      (provided
       (moves-of-pos-simple  :coord :board) => {:path1 :bd1}
       (moves-of-pos-complex :coord :board) => {:path2 :bd2, :path3, :bd3}))

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
      (moves (new-board :b
                        . . 
                        . x))
      => {[[1 1] [0 0]] (new-board :w
                                   K .
                                   . .)})

(fact "moves: itest simple mv on 3x3"
      (moves (new-board :b
                        . . .
                        . . .
                        . . x))
      => {[[2 2] [1 1]] (new-board :w
                                   . . .
                                   . x .
                                   . . .)})

(fact "moves: itest simple mv, left or right"
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

(fact "moves: itest can move backwards if king"
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

(future-fact "moves: itest jump simple"
      (moves (new-board :b
                        . . .
                        . o .
                        . . x)
             => {[[2 2] [0 0]]
                 (new-board :w
                            x . .
                            . . .
                            . . .)}))

(println "--------- END OF CHECKERS ----------" (java.util.Date.))
