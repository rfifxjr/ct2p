(ns plotter.core)

(defrecord Position [x, y])
(defrecord PlotterState [position angle color carriage-state])
(def carriage-states {:up :UP, :down :DOWN})
(def colors {:black "черный" :red "красный" :green "зеленый"})


(defn draw-line [prt from to color]
  (prt (str "...Чертим линию из (" (:x from) ", " (:y from) ") в ("
            (:x to) ", " (:y to) ") используя " color " цвет")))

(defn calc-new-position [distance angle current]
  (let [angle-in-rads (* angle (/ Math/PI 180.0))
        x (+ (:x current) (* distance (Math/cos angle-in-rads)))
        y (+ (:y current) (* distance (Math/sin angle-in-rads)))]
    (->Position (Math/round x) (Math/round y))))


(defn move [prt distance state]
  (let [new-position (calc-new-position distance (:angle state) (:position state))]
    (when (= (:carriage-state state) (:down carriage-states))
      (draw-line prt (:position state) new-position (:color state)))
    (-> state
        (assoc :position new-position))))

(defn turn [prt angle state]
  (prt (str "Поворачиваем на " angle " градусов"))
  (let [new-angle (mod (+ (:angle state) angle) 360.0)]
    (assoc state :angle new-angle)))

(defn carriage-up [prt state]
  (prt "Поднимаем каретку")
  (assoc state :carriage-state (:up carriage-states)))

(defn carriage-down [prt state]
  (prt "Опускаем каретку")
  (assoc state :carriage-state (:down carriage-states)))

(defn set-color [prt color state]
  (prt (str "Устанавливаем " color " цвет линии."))
  (assoc state :color color))

(defn set-position [prt position state]
  (prt (str "Устанавливаем позицию каретки в (" (:x position) ", " (:y position) ")."))
  (assoc state :position position))

(defn draw-triangle [prt size state]
  (-> state
      (carriage-down prt)
      (move prt size)
      (turn prt 120.0)
      (move prt size)
      (turn prt 120.0)
      (move prt size)
      (turn prt 120.0)
      (carriage-up prt)))

(defn draw-square [prt size state]
  (-> state
      (carriage-down prt)
      (move prt size)
      (turn prt 90.0)
      (move prt size)
      (turn prt 90.0)
      (move prt size)
      (turn prt 90.0)
      (move prt size)
      (turn prt 90.0)
      (carriage-up prt)))

(defn initialize-plotter-state [position angle color carriage-state]
  (->PlotterState position angle color carriage-state))

(def printer println)

(defn -main []
  (let [init-position (->Position 0.0 0.0)
        init-color (:black colors)
        init-angle 0.0
        init-carriage-state (:up carriage-states)
        plotter-state (initialize-plotter-state init-position init-angle init-color init-carriage-state)]
    (-> plotter-state
        (draw-triangle printer 100.0)
        (set-position printer (->Position 10.0 10.0))
        (set-color printer (:red colors))
        (draw-square printer 80.0))))
