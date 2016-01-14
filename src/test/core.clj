(ns test.core)

; Q1
;
; Given a string return the set of unique vowels
;
(def vowels '(\a \e \i \o \u))

(defn vowel? [c]
  (some (fn [v] (= c v)) vowels)
  )


(defn string->vowels [string]
  (distinct (filter vowel? string))
  )

(println (string->vowels "cadence"))

; Q2
;
; Given a tree with arbitrary number of children at each node count the total
; number of nodes
;
(defn node-count [tree]
  (count (tree-seq map? (fn [c] (:children c)) tree))
  )


(def tree {:name "a" :children
                 [{:name "b" :children []}
                  {:name "c" :children
                         [{:name "d" :children []}
                          {:name "e" :children
                                 [{:name "g" :children []}]}
                          {:name "f" :children []}
                          ]
                   }
                  ]
           }
  )


(println (node-count tree))


; Q3
;
; Determine if a potentially larger rectangle occludes (including touching) another smaller
; rectangle

(defrecord Point [x y])
(defrecord Rectangle [ll ur])
(def small (Rectangle. (Point. 0 0) (Point. 10 10)))
(def large (Rectangle. (Point. 0 0) (Point. 20 20)))

(defn inside [s b]
  (and (>= (:x (:ll s)) (:x (:ll b)))
       (>= (:y (:ll s)) (:y (:ll b)))
       (<= (:x (:ur s)) (:x (:ur b)))
       (<= (:y (:ur s)) (:y (:ur b)))
       ))

(println (inside small large))
(println (inside large small))



; Q4
;
; Determine teh distance between 2 cartesian coordinates
;
(defn distC [p1 p2]
  (let [xdist (- (:x p1) (:x p2))
        ydist (- (:y p1) (:y p2))
        s (+ (* xdist xdist) (* ydist ydist))
        ]
    (Math/sqrt s)
    )
  )
(println (distC (Point. 0 0 ) (Point. 10 10 )))


; Q5
; Create an abstract interface that "allows" both cartesian and polar coordinates to implement a single function called distance
; Create a new typer Polar Point
; Create a new protocol AllPoints with a method distance
; extend polar and cartesion points to use this protocol

(defrecord PointPolar [a, d])

(defn to-cartesion [p]
  (let
    [x (* (.d p) (Math/cos (Math/toRadians (.a p))))
     y (* (.d p) (Math/sin (Math/toRadians (.a p))))
     ]
    (Point. x y)))

(defprotocol AllPoints
  "Determine distance between 2 coordinates"
  (distance [this other])
  )

(extend-protocol AllPoints Point
  (distance [this other]
    (distC this other)))

(extend-protocol AllPoints PointPolar
  (distance [this other]
    (distC (to-cartesion this) (to-cartesion other))))

(def pp1 (PointPolar. 0 1))
(def pp2 (PointPolar. 90 1))
(def cp1 (Point. 1 0))
(def cp2 (Point. 0 1))

(println "polar" (distance pp1 pp2))
(println "cartesian"(distance cp1 cp2))




; Q6
;
; Given a collection, return a collection of all pairs of items in the original collection
;
(defn all-pairs [xs]
  (when xs
    (concat (map vector (repeat (first xs)) (rest xs))
            (all-pairs (next xs))
            )
    )
  )


; Q7
;
; Vectorize a cicle or radius r at point (x, y) into n vertices
;
(defn vectorize-circle [x y r n]
  (let
    [alpha (/ 360 n)]
    (loop
      [c 0
       points []
       ]
      (if (= c n)
        points
        (let
          [a (Math/toRadians (* alpha c))
           dx (+ x (* r (Math/cos a)))
           dy (+ y (* r (Math/sin a)))]

          (recur (inc c)
                 (conj points (Point. dx dy))))))))


(println (vectorize-circle 0 10 10 4))

; Q8
;
; Determine what is the probability of throwing a 9 on two die
; instead of a closed form solution, use "big data" ergo simulate it.
;
(def die '(1 2 3 4 5 6))
(def attempts 10000000)

(defn toss []
  (+ (rand-nth die)
     (rand-nth die)
     ))

(defn monte-carlo [attempts target]
  (loop
    [t attempts
     good 0]
    (if (= t 0)
      good
      (recur (dec t) (if (= (toss) target) (inc good) good)))))

(def target 9)
(println (float (/ (monte-carlo attempts target) attempts)) (float (/ 1 target)))






