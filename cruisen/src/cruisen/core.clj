(ns cruisen.core
  (:gen-class))

(def state (atom {}))

(defn pos [x y]
  "return coordinate-map"
  {:x x, :y y})

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

(defn collision [state nxt]
  (let [obstacles (get-in state [:obstacles])
        next-step (assoc (get-in state [:rover :position]) :y nxt)]
    (if (contains? obstacles next-step)
      (do (assoc state :status (format "obstacle ahead at: %s" next-step)) true))
      false))



(defn move-tmp [state stp]
  (let [rover-x (get-in state [:rover :position :x])
        rover-y (get-in state [:rover :position :y])]
    (case (get-in state [:rover :direction])
      :N (pos rover-x (- rover-y stp))
      :E (pos () rover-y)
      :S (pos rover-x ())
      :W (pos () rover-y))))

(defn move [state x]
  (let [rover-d (get-in state [:rover :direction])
        rover-p (get-in state [:rover :position])
        height (dec (get-in state [:height]))
        width (dec (get-in state [:width]))]
    (case x
      \f (case rover-d
           :N (if (= (get rover-p :y) 0)
                (update-in state [:rover :position :y] + height)
                (update-in state [:rover :position :y] dec))
           :E (if (= (get rover-p :x) width)
                (update-in state [:rover :position :x] - width)
                (update-in state [:rover :position :x] inc))
           :S (if (= (get rover-p :y) height)
                (update-in state [:rover :position :y] - height)
                (update-in state [:rover :position :y] inc))
           :W (if (= (get rover-p :x) 0)
                (update-in state [:rover :position :x] + width)
                (update-in state [:rover :position :x] dec)))
      \b (case rover-d
           :N (if (= (get rover-p :y) height)
                (update-in state [:rover :position :y] - height)
                (update-in state [:rover :position :y] inc))
           :E (if (= (get rover-d :x) 0)
                (update-in state [:rover :position :x] + width)
                (update-in state [:rover :position :x] dec))
           :S (if (= (get rover-d :y) 0)
                (update-in state [:rover :position :y] + height)
                (update-in state [:rover :position :y] dec))
           :W (if (= (get rover-d :x) width)
                (update-in state [:rover :position :x] - width)
                (update-in state [:rover :position :x] inc))))))

(defn new-d [state d]
  "update direction of rover"
  (assoc-in state [:rover :direction] d))

(defn new-r-wrap [r-idx]
  "correct index to be valid inside direction set"
  (cond
    (< r-idx 0) (+ r-idx 4)
    (> r-idx 3) (- r-idx 4)
    :else r-idx))

(defn indexes-of [e coll]
  "returns index of occurrence"
  (first (keep-indexed #(if (= e %2) %1) coll)))

(defn rotate [state x]
  (let [r-map (list :N :E :S :W)]
    (case (get-in state [:rover :direction])
             :N (new-d state (nth r-map (new-r-wrap (+ (indexes-of :N r-map) x))))
             :E (new-d state (nth r-map (new-r-wrap (+ (indexes-of :E r-map) x))))
             :S (new-d state (nth r-map (new-r-wrap (+ (indexes-of :S r-map) x))))
             :W (new-d state (nth r-map (new-r-wrap (+ (indexes-of :W r-map) x)))))))

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
(defmethod hello \l [state _] (rotate state -1))
(defmethod hello \r [state _] (rotate state 1))
(defmethod hello :default [state x] (assoc state :status (format "No matching case implemented for \"%s\"...yet. Use l,r,f or b instead." x)))

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
(hello-rover! "l")