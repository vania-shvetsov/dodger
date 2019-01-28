(ns dodger.core
  (:require [quil.core :as q]
            [quil.middleware :refer [fun-mode pause-on-error]]))

(defn draw-state [state]
  (q/background 0))

(defn update-state [state]
  state)

(defn setup-sketch []
  (q/frame-rate 60)
  (q/color-mode :rgb)
  {})

(defn on-key-typed [state event])

(q/defsketch dodger
  :title "Dodger Game"
  :size [500 500]
  :setup setup-sketch
  :update update-state
  :draw draw-state
  :key-typed on-key-typed
  :features [:keep-on-top :no-bind-output]
  :middleware [fun-mode ])
