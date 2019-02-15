(ns cruisen.core
  (:gen-class))

(def state (atom {}))

(defn pos [x y]
  "expects two lonely coordinates and brings them into a map."
  {:x x, :y y})

(defn get-obstacle-positions [height width amount-of-obstacles]
  "randomly places the already defined obstacles on the board."
  (let [entire-board (for [x (range width)
                           y (range height)]
                       (pos x y))]
    (take amount-of-obstacles (shuffle (next entire-board)))))

(defn pretty-print [value]
  "maps a given value to representative chars."
  (case value
    :N \∧
    :S \∨
    :W \<
    :E \>
    :obstacles \X
    nil \-
    value))

(defn print-board! []
  "prints board state to console."
  (let [{:keys [height width obstacles rover]} @state]
    (doseq [y (range height)]
      (doseq [x (range width)]
        (let [value
              (if (get obstacles (pos x y)) :obstacles
                  (if (= (get rover :position) (pos x y)) (get rover :direction) nil))]
          (print (pretty-print value))))
      (newline))))

(defn collision! [state tmp-step]
  "checks if tmp-step collides with obstacle and prints the position of the obstacle."
  (let [obstacles (get-in state [:obstacles])]
    (if (contains? obstacles tmp-step)
      (do (println (format "obstacle ahead at: %s" tmp-step)) state)
      (assoc-in state [:rover :position] tmp-step))))

(defn tmp-move [state step]
  "calculates tmp-move without considering board borders."
  (let [rover-x (get-in state [:rover :position :x])
        rover-y (get-in state [:rover :position :y])]
    (case (get-in state [:rover :direction])
      :N (pos rover-x (- rover-y step))
      :E (pos (+ rover-x step) rover-y)
      :S (pos rover-x (+ rover-y step))
      :W (pos (- rover-x step) rover-y))))

(defn wrap-move [new-pos state]
  "handles wrap around of rover."
  (let [height (get-in state [:height])
        width (get-in state [:width])]
    (cond
      (< (new-pos :y) 0) (update new-pos :y + height)
      (> (new-pos :y) (dec height)) (update new-pos :y - height)
      (< (new-pos :x) 0) (update new-pos :x + width)
      (> (new-pos :x) (dec width)) (update new-pos :x - width)
      :else new-pos)))

(defn move [state step]
  "chaining all necessary operations. conditionals are handled inside the functions who is in charge of the current operation."
  (collision! state (wrap-move (tmp-move state step) state)))

(defn new-d [state d]
  "update direction of rover."
  (assoc-in state [:rover :direction] d))

(defn new-r-wrap [r-idx]
  "correct index to be valid inside direction set."
  (cond
    (< r-idx 0) (+ r-idx 4)
    (> r-idx 3) (- r-idx 4)
    :else r-idx))

(defn indexes-of [e coll]
  "returns index of first occurrence of e in collection."
  (first (keep-indexed #(if (= e %2) %1) coll)))

(defn rotate [state x]
  "calculates and handles direction update of rover."
  (let [r-map (list :N :E :S :W)]
    (case (get-in state [:rover :direction])
             :N (new-d state (nth r-map (new-r-wrap (+ (indexes-of :N r-map) x))))
             :E (new-d state (nth r-map (new-r-wrap (+ (indexes-of :E r-map) x))))
             :S (new-d state (nth r-map (new-r-wrap (+ (indexes-of :S r-map) x))))
             :W (new-d state (nth r-map (new-r-wrap (+ (indexes-of :W r-map) x)))))))

(defn init!
  "initial board creation.
  default values are applied if no values are provided."
  ([] (init! 10 20 (* 10 20 0.1)))
  ([height width amount-of-obstacles]
   (reset! state {:height    height
                  :width     width
                  :rover     {:position {:x 0 :y 0} :direction :N}
                  :obstacles (set (get-obstacle-positions height
                                                          width
                                                          amount-of-obstacles))})
   (print-board!)))

(defn no-match! [state x]
"handles feedback in case of invalid input characters."
  (do (println (format "No matching case implemented for \"%s\"...yet. \nUse l,r,f or b instead." x)) state))

(defmulti hello (fn [_ x] x))
(defmethod hello \f [state _] (move state 1))
(defmethod hello \b [state _] (move state -1))
(defmethod hello \l [state _] (rotate state -1))
(defmethod hello \r [state _] (rotate state 1))
(defmethod hello :default [state x] (no-match! state x))

(defn hello-rover [state msg]
  "walks through the cmd sequence for the rover."
  (if (empty? msg)
    state
    (let [new-state (hello state (first msg))]
      (recur new-state (rest msg)))))

(defn hello-rover! [msg]
  "gets the state and prints the board after all operations are done."
  (swap! state hello-rover msg)
  (print-board!))

; Init and first test
;(init!)
;(hello-rover! "fffxf")