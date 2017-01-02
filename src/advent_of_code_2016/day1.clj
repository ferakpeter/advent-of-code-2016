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