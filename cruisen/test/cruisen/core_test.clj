(ns cruisen.core-test
  (:require [clojure.test :refer :all]
            [clojure.spec :as s]
            [clojure.spec.test :as st]
            [cruisen.core :refer :all]))

(deftest rover-tests
  (testing "rover moves forward with f command"
    (is (= (move {:x 0 :y 0 :d \N} "f") {:x 0 :y 0 :d \N}))))

(defn move [rover commands] rover)

(testing "rover does not accept invalid commands")

(is (thrown? Exception (monve {{:x 0 :y 0 :d \N}} nil)))

(s/fdef move :args (s/cat :rover any? :command string?))
(st/instrument `move)

(is (thrown? Exception (monve {{:x 0 :y 0 :d \N}} "")))

(s/def ::commands (s/and string? #(re-matches #"^[f]+$" %)))

(is (thrown? Exception (monve {{:x 0 :y 0 :d \N}} "fffgg")))

(testing "rover does not accept an invalid position")

(is (thrown? Exeception (move nil "f")))

;(is (thrown? Exeception (move {:x 0} "f")))
;(is (thrown? Exeception (move {:z 0 :y 0 :d \N} "f")))
;(is (thrown? Exeception (move nil "f")))
;(is (thrown? Exeception (move nil "f")))
;(is (thrown? Exeception (move nil "f")))

(s/def ::x int?)
(s/def ::y int?)
(s/def ::d (s/and keyword? #(= :N %)))
(s/def ::rover (s/keys :req-un [::x ::y ::d]))
