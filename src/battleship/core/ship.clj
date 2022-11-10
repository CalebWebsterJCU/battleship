(ns battleship.core.ship
  (:require [clojure.set :as set]))

(defn ship-occupies-cell? [x-y-pair ship] (contains? ship x-y-pair))

(defn get-common-cells [ship1 ship2] (set/intersection ship1 ship2))

(defn do-ships-collide? [ship1 ship2] (seq (get-common-cells ship1 ship2)))

(defn get-ships-that-collide [ship ships-to-check] (filter #(do-ships-collide? ship %) ships-to-check))

(defn get-first-collision [ship other-ships] (first (get-ships-that-collide ship other-ships)))

(defn does-ship-collide-with-others? [ship other-ships] (seq (get-ships-that-collide ship other-ships)))

(defn throw-ship-collision-exception [ship1 ship2]
  (throw (IllegalArgumentException. (str "Cannot place ship at " ship1 "; collides with ship at " ship2)))
  )

(defn throw-exception-with-first-collision-found [ship other-ships]
  (throw-ship-collision-exception ship (get-first-collision ship other-ships))
  )
