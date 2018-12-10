(ns cruisen.core
  (:gen-class))

;(defn -main
;  "I don't do a whole lot ... yet."
;  [& args]
;  (println "Hello, World!"))

(def state (atom {{} :obstacles
                  :width     30
                  :height    16
                  :rover     {:x 0 :y 0}
                  :status    :fine
                  }))

(defn pretty-print [value]
  (case value
    :obstacles \X
    nil \-
    :rover \O
    value))

(defn print-board! []
  (let [{:keys [height width obstacles]} @state]
    (doseq [y (range height)]
      (doseq [x (range width)]
        (let [value
              (if (get obstacles (pos x y)) :obstacles
                    (if (= (get @state :rover) (pos x y)) :rover nil))]
          (print (pretty-print value))))
      (newline))))

(defn pos [x y] {:x x, :y y})

(defn get-obstacle-positions [height width amount-of-obstacles]
  (let [entire-board (for [x (range width)
                           y (range height)]
                      (pos x y))]
    (take amount-of-obstacles (shuffle entire-board))))

(defn init!
  ([] (init! 16 30 (* 16 30 0.1)))
  ([height width amount-of-obstacles]
   (reset! state {:obstacles (set (get-obstacle-positions height
                                                          width
                                                          amount-of-obstacles))
                  :width     width
                  :height    height
                  :rover     {:x 0 :y 0}
                  :status    :fine})
   (print-board!)))

(init!)


