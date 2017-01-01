(ns advent-of-code-2016.core
  (:require
    [clojure.java.io :as io]))

(def day1 (read-string (str "[" (slurp (io/resource "day1")) "]")))

(def left {:north :west, :west :south, :south :east, :east :north})
(def right {:north :east, :east :south, :south :west, :west :north})

(defn understand-direction [string-direction]
  (if (= string-direction "L") left right))

(defn move-x [direction value steps]
  (if (= direction :west) (- value steps) (if (= direction :east) (+ value steps) value)))

(defn move-y [direction value steps]
  (if (= direction :south) (- value steps) (if (= direction :north) (+ value steps) value)))

(defn movenext [state step]
  (def new-direction ((understand-direction (str (first step))) (state :direction)))
  (def steps (repeat (Long/parseLong (apply str (next step))) 1))
  (map #({ :direction new-direction, :x (move-x new-direction (state :x) steps), :y (move-y new-direction (state :y) steps) }) steps))

(defn distance [point]
  (+ (Math/abs (point :x)) (Math/abs (point :y))))

(defn part-one [d start]
  (->>
   d
   (reduce movenext start)
   (distance)))

(defn part-two [d start]
  (def values (reductions movenext (list start) d))

  (def end
    (reduce (fn [acc step]
              (let [position (select-keys step [:x :y])]

              (if (contains? acc position)
                (reduced position)
                (conj acc position))))
            #{}
            values))
  end)

(defn -main []
   (println (part-two (map name day1) { :direction :north, :x 0, :y 0 } )))

(part-one (map name day1) { :direction :north, :x 0, :y 0 })
(part-two (map name day1) { :direction :north, :x 0, :y 0 })

; DAY 2

(def day2 (clojure.string/split-lines (slurp (io/resource "day2"))))

(def dial [(into (sorted-set) (range 1 4)) (into (sorted-set) (range 4 7)) (into (sorted-set) (range 7 10))])
(def moves { :U -3 :D 3 :L -1 :R 1})

(defn abs [n] (max n (- n)))

(defn  index [longest row-length column]
  (- column (quot (- longest row-length) 2)))

(index 5 1 2)

;(defn add-column-value [acc 

(defn select-column [longest dial column]
  (reduce (fn [acc curr]
            (def i (index longest (count curr) column))
            (println i)
            (into acc (vector (get (into []  curr) i))))
          #{}
          dial))

(defn longest [high current]
  (if (< high (count current)) (count current) high))

(defn get-valid-values [value step]
  (def current-row (into #{} (first (filter #(contains? % value) dial))))
  (def position (.indexOf (into [] current-row) value))
  (def longest-row (reduce longest 0 dial))
  (if (= (abs step) 1)
    current-row
    (select-column longest-row dial position)))

(defn move [acc step]
  (def value (+ acc step))
  (if (contains? (get-valid-values acc step) value) value acc))

(defn con [line]
  (map #(moves (keyword (str %))) line))

(defn move-list [acc line]
  (reduce move acc line))

(def test (list "ULL" "RRDDD" "LURDL" "UUUUD"))

(clojure.string/join (next (reductions move-list 5 (map con day2))))
; 98575



(next (reductions move-list 5 (map con test)))
; 1985

(def dial-characters {:10 "A" :11 "B" :12 "C" :13 "D" }
(def dial2 [#{1} (into (sorted-set) (range 2 5)) (into (sorted-set) (range 5 10)) (into (sorted-set) (range 10 13)) #{13}])

                                        ; test data with dial2 expects 5DB3

(select-column 3 dial 2)

