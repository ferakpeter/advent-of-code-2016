(ns advent-of-code-2016.core
  (:require
    [clojure.java.io :as io]))

(def day2 (clojure.string/split-lines (slurp (io/resource "day2"))))

;(def dial [(into (sorted-set) (range 1 4)) (into (sorted-set) (range 4 7)) (into (sorted-set) (range 7 10))])
;(clojure.string/join (next (reductions move-list 5 (map con day2))))
; 98575

(def part1-rows ['(1 2 3) '(4 5 6) '(7 8 9)])
(def part1-columns ['(1 4 7) '(2 5 8) '(3 6 9)])

(def part2-rows ['(1) '(2 3 4) '(5 6 7 8 9) '(10 11 12) '(13)])
(def part2-columns ['(5) '(2 6 10) '(1 3 7 11 13) '(4 8 12) '(9)])
(def part2-characters {:10 "A" :11 "B" :12 "C" :13 "D" }

(defn get-next [target current next]
  (if (= current target) (reduced next) next))

(defn moves [rows cols]
  { :U (map reverse cols) :D cols :L (map reverse rows) :R rows }))
(def part1-moves (moves part1-rows part1-cols))
(def part2-moves (moves part2-rows part2-cols))

(defn values [dial position move-type]
  (first (filter #(some (set (vector position)) %) dial)))

(defn move-through-dial [move-sequence moves start-key]
  (next (reductions #(reduce (fn [position move-type]
                (reduce (fn [c n] (get-next position c n))
                        (values (move-sequence (keyword move-type)) position move-type)))
              %1
              (map str %2))
       start-key
       moves)))

(defn part1 []
  (move-through-dial part1-moves day2 5))
; 98575

(defn part2 []
  (move-through-dial part2-moves day2 5))
; 12 13 8 13 4
; C D 8 D 4

(def test (list "ULL" "RRDDD" "LURDL" "UUUUD"))
(move-through-dial part1-moves test 5)
; test data with part1 expects 1985
; test data with part2 expects 5DB3

(defn -main []
   (println (clojure.string/join (part-two))))


 ; (next (reductions #(reduce (fn [position move-type]
  ;              (reduce (fn [c n] (get-next position c n))
  ;                      (values (moves (keyword move-type)) position move-type)))
  ;            %1
   ;           (map str %2))
    ;   5
    ;   day2)))
