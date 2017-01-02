(ns advent-of-code-2016.day1
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

(defn movenext [s step]
  (def state (last s))
  (def new-direction ((understand-direction (str (first step))) (state :direction)))
  (def steps (repeat (Long/parseLong (apply str (next step))) 1))

  (reductions #(zipmap [:direction :x :y]
                       [new-direction
                        (move-x new-direction (%1 :x) %2)
                        (move-y new-direction (%1 :y) %2)])
       state
       steps))

(defn distance [point]
  (+ (Math/abs (point :x)) (Math/abs (point :y))))

(defn part1 [d start]
  (->>
   d
   (reduce movenext [start])
   (last)
   (distance)))

(defn part2 [d start]
  (def values (next (reduce movenext [start] d)))
  (println values)

    (reduce (fn [acc step]
              (let [position (select-keys step [:x :y])]

              (if (contains? acc position)
                (reduced position)
                (conj acc position))))
            #{}
            values))

(defn -main []
   (println (part-two (map name day1) { :direction :north, :x 0, :y 0 } )))

(def s { :direction :north :x 0 :y 0 })
(part1 (map name day1) s)
(part2 (map name day1) s)
