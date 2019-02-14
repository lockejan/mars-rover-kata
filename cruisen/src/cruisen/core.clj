(ns cruisen.core
  (:gen-class))

(def state (atom {}))

(defn pos [x y] {:x x, :y y})

(defn get-obstacle-positions [height width amount-of-obstacles]
  (let [entire-board (for [x (range width)
                           y (range height)]
                       (pos x y))]
    (take amount-of-obstacles (shuffle entire-board))))

(defn pretty-print [value]
  (case value
    :obstacles \X
    nil \-
    :rover \O
    \N \∧
    \S \∨
    \W \<
    \E \>
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
         \N (update-in state [:rover :position :y] dec)
         \E (update-in state [:rover :position :x] inc)
         \S (update-in state [:rover :position :y] inc)
         \W (update-in state [:rover :position :x] dec))
    \b (case (get-in state [:rover :direction])
         \N (update-in state [:rover :position :y] inc)
         \E (update-in state [:rover :position :x] dec)
         \S (update-in state [:rover :position :y] dec)
         \W (update-in state [:rover :position :x] inc))))

(defn rotate [state x]
  (case x
    \r (case (get-in state [:rover :direction])
         \N (assoc-in state [:rover :direction] \E)
         \E (assoc-in state [:rover :direction] \S)
         \S (assoc-in state [:rover :direction] \W)
         \W (assoc-in state [:rover :direction] \N))
    \l (case (get-in state [:rover :direction])
         \N (assoc-in state [:rover :direction] \W)
         \E (assoc-in state [:rover :direction] \N)
         \S (assoc-in state [:rover :direction] \E)
         \W (assoc-in state [:rover :direction] \S))))

(defn init!
  ([] (init! 10 20 (* 10 20 0.1)))
  ([height width amount-of-obstacles]
   (reset! state {:height    height
                  :width     width
                  :rover     {:position {:x 0 :y 0} :direction \S}
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
  (if (empty? msg)
    state
    (let [new-state (hello state (first msg))]
      (recur new-state (rest msg)))))

(defn hello-rover! [msg]
  (swap! state hello-rover msg)
  (print-board!))


; Init and first test


(init!)
;(hello-rover! "fffllrf")
