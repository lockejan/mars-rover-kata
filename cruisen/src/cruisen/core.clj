(ns cruisen.core
  (:gen-class)
  (:require [clojure.spec.alpha :as s]))

(def state (atom {}))

(defn pos [x y] {:x x, :y y})

(defn get-obstacle-positions [height width amount-of-obstacles]
  (let [entire-board (for [x (range width)
                           y (range height)]
                       (pos x y))]
    (take amount-of-obstacles (shuffle (next entire-board)))))

(defn pretty-print [value]
  (case value
    :N \∧
    :S \∨
    :W \<
    :E \>
    :obstacles \X
    nil \-
    value))

(defn print-board! []
  (let [{:keys [height width obstacles rover]} @state]
    (doseq [y (range height)]
      (doseq [x (range width)]
        (let [value
              (if (get obstacles (pos x y)) :obstacles
                  (if (= (get rover :position) (pos x y)) (get rover :direction) nil))]
          (print (pretty-print value))))
      (newline))))

(defn move [state x]
  (case x
    \f (case (get-in state [:rover :direction])
         :N (if (= (get-in state [:rover :position :y]) 0)
              (update-in state [:rover :position :y] + (dec (get-in state [:height])))
              (update-in state [:rover :position :y] dec))
         :E (if (= (get-in state [:rover :position :x]) (dec (get-in state [:width])))
              (update-in state [:rover :position :x] - (dec (get-in state [:width])))
              (update-in state [:rover :position :x] inc))
         :S (if (= (get-in state [:rover :position :y]) (dec (get-in state [:height])))
              (update-in state [:rover :position :y] - (dec (get-in state [:height])))
              (update-in state [:rover :position :y] inc))
         :W (if (= (get-in state [:rover :position :x]) 0)
              (update-in state [:rover :position :x] + (dec (get-in state [:width])))
              (update-in state [:rover :position :x] dec)))
    \b (case (get-in state [:rover :direction])
         :N (if (= (get-in state [:rover :position :y]) (dec (get-in state [:height])))
              (update-in state [:rover :position :y] - (dec (get-in state [:height])))
              (update-in state [:rover :position :y] inc))
         :E (if (= (get-in state [:rover :position :x]) 0)
              (update-in state [:rover :position :x] + (dec (get-in state [:width])))
              (update-in state [:rover :position :x] dec))
         :S (if (= (get-in state [:rover :position :y]) 0)
              (update-in state [:rover :position :y] + (dec (get-in state [:height])))
              (update-in state [:rover :position :y] dec))
         :W (if (= (get-in state [:rover :position :x]) (dec (get-in state [:width])))
              (update-in state [:rover :position :x] - (dec (get-in state [:width])))
              (update-in state [:rover :position :x] inc)))))

(defn rotate [state x]
  (case x
    \r (case (get-in state [:rover :direction])
         :N (assoc-in state [:rover :direction] :E)
         :E (assoc-in state [:rover :direction] :S)
         :S (assoc-in state [:rover :direction] :W)
         :W (assoc-in state [:rover :direction] :N))
    \l (case (get-in state [:rover :direction])
         :N (assoc-in state [:rover :direction] :W)
         :E (assoc-in state [:rover :direction] :N)
         :S (assoc-in state [:rover :direction] :E)
         :W (assoc-in state [:rover :direction] :S))))

(defn init!
  ([] (init! 10 20 (* 10 20 0.1)))
  ([height width amount-of-obstacles]
   (reset! state {:height    height
                  :width     width
                  :rover     {:position {:x 0 :y 0} :direction :N}
                  :status    :fine
                  :obstacles (set (get-obstacle-positions height
                                                          width
                                                          amount-of-obstacles))})
   (print-board!)))

(defmulti hello (fn [_ x] x))
(defmethod hello \f [state x] (move state x))
(defmethod hello \b [state x] (move state x))
(defmethod hello \l [state x] (rotate state x))
(defmethod hello \r [state x] (rotate state x))
(defmethod hello :default [_ _] (println "No matching case implemented...yet"))

(defn hello-rover [state msg]
  ;(println state)
  (println (get-in state [:rover :position]) (get-in state [:height]))
  (if (empty? msg)
    state
    (let [new-state (hello state (first msg))]
      (recur new-state (rest msg)))))

(defn hello-rover! [msg]
  (swap! state hello-rover msg)
  (print-board!))


; Init and first test
(init!)
(hello-rover! "bbbbb")