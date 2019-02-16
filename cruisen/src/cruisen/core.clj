(ns cruisen.core
  (:gen-class))

(def state (atom {}))

(defn- pos
  "Expects two lonely coordinates and brings them into a map."
  [x y]
  {:x x, :y y})

(defn- get-obstacle-positions
  "Randomly places the already defined obstacles on the board."
  [height width amount-of-obstacles]
  (let [entire-board (for [x (range width)
                           y (range height)]
                       (pos x y))]
    (take amount-of-obstacles (shuffle (next entire-board)))))

(defn- pretty-print
  "Maps a given value to representative chars."
  [value]
  (case value
    :N \∧
    :S \∨
    :W \<
    :E \>
    :obstacles \X
    nil \-
    value))

(defn print-board!
  "Prints board state to console.
   Should be called without without parameters."
  []
  (let [{:keys [height width obstacles rover]} @state]
    (doseq [y (range height)]
      (doseq [x (range width)]
        (let [value
              (if (get obstacles (pos x y)) :obstacles
                  (if (= (get rover :position) (pos x y)) (get rover :direction) nil))]
          (print (pretty-print value))))
      (newline))))

(defn- collision
  "Checks if tmp-step collides with obstacle and prints the position of the obstacle."
  [state tmp-step]
  (let [obstacles (get-in state [:obstacles])]
    (if (contains? obstacles tmp-step)
      (assoc-in state [:status] {:error (format "Obstacle ahead at: %s" tmp-step)})
      (assoc-in state [:rover :position] tmp-step))))

(defn- tmp-move
  "Calculates tmp-move without considering board borders."
  [state step]
  (let [rover-x (get-in state [:rover :position :x])
        rover-y (get-in state [:rover :position :y])]
    (case (get-in state [:rover :direction])
      :N (pos rover-x (- rover-y step))
      :E (pos (+ rover-x step) rover-y)
      :S (pos rover-x (+ rover-y step))
      :W (pos (- rover-x step) rover-y))))

(defn- wrap-move
  "Handles wrap around of rover."
  [new-pos state]
  (let [height (get-in state [:height])
        width (get-in state [:width])]
    (cond
      (< (new-pos :y) 0) (update new-pos :y + height)
      (> (new-pos :y) (dec height)) (update new-pos :y - height)
      (< (new-pos :x) 0) (update new-pos :x + width)
      (> (new-pos :x) (dec width)) (update new-pos :x - width)
      :else new-pos)))

(defn- move
  "Chaining all necessary operations to move the rover.
  Conditionals are handled inside the functions which is in charge of the current operation."
  [state step]
  (collision state (wrap-move (tmp-move state step) state)))

(defn- new-d
  "Update direction of rover."
  [state new-d]
  (assoc-in state [:rover :direction] new-d))

(defn- new-r-wrap
  "Correct index to be valid inside direction set."
  [r-idx]
  (cond
    (< r-idx 0) (+ r-idx 4)
    (> r-idx 3) (- r-idx 4)
    :else r-idx))

(defn- indexes-of
  "Returns index of first occurrence of elem in collection."
  [e coll]
  (first (keep-indexed #(if (= e %2) %1) coll)))

(defn- rotate
  "Calculates and handles direction update of rover."
  [state x]
  (let [r-map (list :N :E :S :W)]
    (case (get-in state [:rover :direction])
      :N (new-d state (nth r-map (new-r-wrap (+ (indexes-of :N r-map) x))))
      :E (new-d state (nth r-map (new-r-wrap (+ (indexes-of :E r-map) x))))
      :S (new-d state (nth r-map (new-r-wrap (+ (indexes-of :S r-map) x))))
      :W (new-d state (nth r-map (new-r-wrap (+ (indexes-of :W r-map) x)))))))

(defn init!
  "Initial board creation.
  Accepts two integer values to define the board size.
    Example call: (init! 10 20)
  Default values are applied if no values are provided."
  ([] (init! 10 20))
  ([height width]
   (reset! state {:height    height
                  :width     width
                  :status    {:fine "Nothing suspicious"}
                  :rover     {:position {:x 0 :y 0} :direction :N}
                  :obstacles (set (get-obstacle-positions height
                                                          width
                                                          (* height width 0.1)))})
   (print-board!)))

(defn- no-match
  "Handles feedback in case of invalid input characters."
  [state x]
  (assoc-in state [:status] {:error (format "No matching case implemented for \"%s\"...yet. \nUse l,r,f or b instead." x)}))

(defmulti hello (fn [_ x] x))
(defmethod hello \f [state _] (move state 1))
(defmethod hello \b [state _] (move state -1))
(defmethod hello \l [state _] (rotate state -1))
(defmethod hello \r [state _] (rotate state 1))
(defmethod hello :default [state x] (no-match state x))

(defn- check-error [state msg]
  (if (contains? (get-in state [:status]) :error)
    nil
    msg))

(defn- print-error! []
  (let [{:keys [status]} @state]
    (if (contains? status :error)
      (println (get status :error))
      nil)))

(defn- hello-rover
  "Walks through the cmd sequence for the rover, char by char."
  [state msg]
  (if (empty? msg)
    state
    (let [new-state (hello state (first msg))]
      (recur new-state (rest (check-error state msg))))))

(defn hello-rover!
  "Accepts a character sequence.
  Valid chars are \"l\", \"r\", \"f\" and \"b\".
  \"f\" moves the rover forwards, \"b\" moves it backwards.
  \"l\" let's it rotate counter-clockwise \"b\" clockwise.
  Gets the state and prints the board after all operations are done."
  [msg]
  (swap! state hello-rover msg)
  (print-board!)
  (print-error!)
  (let [{:keys [status]} @state]
    (if (contains? status :error)
      (do (swap! state assoc :status {:fine "Nothing suspicious"}) nil))))

; Init and first test
;(init!)
;(hello-rover! "fff")