(ns dodger.core
  (:require [quil.core :as q]
            [quil.middleware :refer [fun-mode pause-on-error]]
            [quil.applet :as applet])
  (:import [java.lang Math]))

(def player-size 40)
(def min-enemy-size 30)
(def max-enemy-size 70)
(def min-enemy-speed 1)
(def max-enemy-speed 3)
(def min-spawn-timer 30)
(def max-spawn-timer 50)

(declare dodger)

(defmacro within [& body]
  `(applet/with-applet dodger ~@body))

(defn in-range? [x a b]
  (<= a x b))

(defn half [x] (/ x 2))

(defn sqrt [x]
  (Math/sqrt x))

(defn square [x]
  (Math/pow x 2))

(defn rand-from [a b]
  (int (+ a
          (* (rand) (inc (- b a))))))

(def key-dir {:w :top
              :s :down
              :a :left
              :d :right})

(defn rand-enemy-size []
  (rand-from min-enemy-size max-enemy-size))

(defn distance [p1 r1 p2 r2]
  (let [[x1 y1] p1
        [x2 y2] p2]
    (- (sqrt (+ (square (- x1 x2))
                (square (- y1 y2))))
       (+ r1 r2))))

(defn collide-entities? [e1 e2]
  (< (distance (:position e1) (half (:size e1))
               (:position e2) (half (:size e2)))
     0.1))

(defn create-player []
  {:position [250 470]
   :sprite (let [img (q/load-image "player.png")]
             (q/resize img 0 player-size)
             img)
   :size player-size
   :speed 6})

(defn create-enemy []
  (let [size (rand-enemy-size)
        half-size (int (half size))
        min-x half-size
        max-x (- (q/width) half-size)
        y (- size)]
    {:position [(rand-from min-x max-x) (- half-size)]
     :sprite (let [img (q/load-image "enemy.png")]
               (q/resize img 0 size)
               img)
     :size size
     :speed (rand-from min-enemy-speed max-enemy-speed)}))

(defn move-player* [player dir]
  (let [{:keys [speed position size]} player
        half-size (half size)
        min-x half-size
        max-x (- (q/width) half-size)
        min-y half-size
        max-y (- (q/height) half-size)
        [x y] position
        [x' y'] (case dir
                  :top [x (- y speed)]
                  :down [x (+ y speed)]
                  :left [(- x speed) y]
                  :right [(+ x speed) y])]
    (assoc player :position [(if (in-range? x' min-x max-x) x' x)
                             (if (in-range? y' min-y max-y) y' y)])))

(defn move-player [state]
  (assoc state :player (reduce move-player*
                               (:player state)
                               (:move-to state))))

(defn move-enemy [enemy]
  (let [{:keys [speed position]} enemy
        [x y] position]
    (assoc enemy :position [x (+ y speed)])))

(defn enemy-is-out? [enemy]
  (let [{:keys [position size]} enemy]
    (> (position 1) (+ 1 (q/height) (/ size 2)))))

(defn move-enemies [state]
  (let [enemies (:enemies state)
        enemies'
        (->> enemies
             (map move-enemy*)
             (remove enemy-is-out?)
             (vec))
        diff (- (count enemies) (count enemies'))
        score' (if (> diff 0)
                 (+ (:score state) diff)
                 (:score state))]
    (assoc state
           :enemies enemies'
           :score score')))

(defn cleanup-move-to [s dir]
  (case dir
    (:top :down) (disj s :top :down)
    (:left :right) (disj s :left :right)))

(defn process-move-key [state key]
  (if (some #(= key %) (keys key-dir))
    (let [dir (key-dir key)]
      (assoc state
             :move-to
             (-> (get state :move-to)
                 (cleanup-move-to dir)
                 (conj dir))))
    state))

(defn check-game-over [state]
  (let [{:keys [player enemies]} state
        has-collision (some #(collide-entities? player %) enemies)]
    (println "Has collision: " has-collision)
    (if has-collision
      (assoc state :game-over true)
      state)))

(defn update-timer [state]
  (update state :timer (fn [timer]
                         (if (<= timer 0)
                           (rand-from min-spawn-timer max-spawn-timer)
                           (dec timer)))))

(defn create-enemy-if-needed [state]
  (let [timer (:timer state)]
    (if (= timer 0)
      (update state :enemies conj (create-enemy))
      state)))

(defn draw-entity [p]
  (let [{:keys [sprite position size]} p
        [x y] position
        half-size (half size)
        x' (- x half-size)
        y' (- y half-size)]
    (q/image sprite x' y')))

(defn initial-state []
  {:player (create-player)
   :enemies [(create-enemy)]
   :move-to #{}
   :timer 100
   :score 0
   :game-over false})

(defn draw-score [score]
  (q/fill 255)
  (q/text-align :left)
  (q/text-size 20)
  (q/text (str "Score: " score) 10 25))

(defn draw-game-over [game-over]
  (when game-over
    (q/fill 50 200 50)
    (q/text-size 40)
    (q/text-align :center)
    (q/text "Game Over :(" (half (q/width)) (half (q/height)))))

(defn draw-state [state]
  (q/background 0)
  (draw-entity (:player state))
  (doseq [enemy (:enemies state)]
    (draw-entity enemy))
  (draw-score (:score state))
  (draw-game-over (:game-over state)))

(defn update-state [state]
  (if (:game-over state)
    state
    (-> state
        (move-player)
        (check-game-over)
        (move-enemies)
        (update-timer)
        (create-enemy-if-needed))))

(defn setup-sketch []
  (q/frame-rate 60)
  (q/color-mode :rgb)
  (initial-state))

(defn on-key-pressed [state event]
  (let [key (:key event)]
    (process-move-key state key)))

(defn on-key-released [state event]
  (let [key (:key event)]
    state
    (update state :move-to disj (key-dir key))))

(q/defsketch dodger
  :title "Dodger Game"
  :size [500 500]
  :setup setup-sketch
  :update update-state
  :draw draw-state
  :key-pressed on-key-pressed
  :key-released on-key-released
  :features [:keep-on-top :no-bind-output]
  :middleware [fun-mode pause-on-error])
