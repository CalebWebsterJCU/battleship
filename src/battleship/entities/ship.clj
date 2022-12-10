(ns battleship.entities.ship
  (:require [clojure.set :as set]
            [battleship.entities.cell :as cell]))

(defn ship-occupies-cell? [x-y-pair ship] (contains? ship x-y-pair))

(defn get-common-cells [ship1 ship2] (set/intersection ship1 ship2))

(defn do-ships-collide? [ship1 ship2] (seq (get-common-cells ship1 ship2)))

(defn get-ships-that-collide [ship ships-to-check] (filter #(do-ships-collide? ship %) ships-to-check))

(defn get-first-collision [ship other-ships] (first (get-ships-that-collide ship other-ships)))

(defn does-ship-collide-with-others? [ship other-ships] (seq (get-ships-that-collide ship other-ships)))

(defn throw-ship-has-no-cells-exception []
  (throw (RuntimeException. "Cannot deploy ship; ship has no cells"))
  )

(defn throw-ship-has-invalid-cells-exception [ship board-size]
  (throw (RuntimeException. (str "Cannot deploy ship at " ship "; contains invalid cells " (clojure.string/join ", " (cell/find-invalid-cells ship board-size))))))

(defn throw-ship-collides-with-others-exception [ship1 other-ships]
  (throw (RuntimeException. (str "Cannot deploy ship at " ship1 "; collides with ships at " other-ships))))

