(ns day2
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

;; 괄호 추가 ctl+option+P
;; 괄호 삭제 ctl+option+s
;; 대괄호 추가 ctl+option+S
;; 네임 스페이스 로드 ctl+option+C

(def input (string/split-lines (slurp (io/resource "input/day2.txt"))))

(defn string+int [[c1, c2]]
  [c1, (Integer/parseInt c2)])

(defn parse [coll]
  (->> coll
       (map (fn [v]
              (string/split v #" ")))
       (map string+int)))


(defn compute [{:keys [horizontal depth]} [cmd, num]]
  (case cmd
    "forward" {:horizontal (+ horizontal num)
               :depth depth}
    "down"    {:horizontal horizontal
               :depth (+ depth num)} 
    "up"      {:horizontal horizontal
               :depth (- depth num)}))


(def result (->> (parse input)
                 (reduce compute {:horizontal 0
                                  :depth 0})))

(prn (* (:horizontal result)
        (:depth result)))

