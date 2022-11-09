(ns battleship.core
  (:require [clojure.set :as set]))

(defn not-empty? [coll] (not (empty? coll)))

(defn every-and-at-least-one? [pred coll] (and (not-empty? coll) (every? pred coll)))

(defn create-ship [name & x-y-pairs] {:name name :cells (set x-y-pairs)})

(defn has-no-ships? [board] (empty? (:ships board)))

(defn has-no-shot-cells? [board] (empty? (:shot-cells board)))

(defn is-board-clear? [board] (and (has-no-ships? board) (has-no-shot-cells? board)))

(defn create-board [size] {:size size :shot-cells #{} :ships #{}})

(defn count-ships [board] (count (:ships board)))

(defn get-common-cells [ship1 ship2] (set/intersection (:cells ship1) (:cells ship2)))

(defn do-ships-collide? [ship1 ship2] (not-empty? (get-common-cells ship1 ship2)))

(defn get-ships-that-collide [ship ships-to-check] (filter #(do-ships-collide? ship %) ships-to-check))

(defn get-first-collision [ship other-ships] (first (get-ships-that-collide ship other-ships)))

(defn does-ship-collide-with-others? [ship other-ships] (not-empty? (get-ships-that-collide ship other-ships)))

(defn throw-ship-collision-exception [ship1 ship2]
  (throw (IllegalArgumentException. (str
                                      "Cannot place ship " (:name ship1) "; collides with " (:name ship2)
                                      " at cells " (get-common-cells ship1 ship2))))
  )

(defn throw-ship-collision-exception-with-first-collision [ship other-ships]
  (throw-ship-collision-exception ship (get-first-collision ship other-ships))
  )

(defn throw-invalid-coordinates-exception [x-y-pairs]
  (throw (IllegalArgumentException. (str "Invalid coordinates; these cells do not exist: " (set x-y-pairs))))
  )

(defn is-coordinate-valid? [coordinate board-size] (and (< coordinate board-size) (>= coordinate 0)))

(defn is-cell-valid? [[x y] board-size] (and (is-coordinate-valid? x board-size) (is-coordinate-valid? y board-size)))

(defn get-invalid-coordinates-of-ship [ship board-size] (filter #(is-cell-valid? % board-size) (:cells ship)))

(defn are-cells-valid? [cells board-size] (every-and-at-least-one? #(is-cell-valid? % board-size) cells))

(defn deploy-ship-if-no-collisions [ship board]
  (let [current-ships (:ships board)]
   (if (not (does-ship-collide-with-others? ship current-ships))
     (assoc board :ships (conj current-ships ship))
     (throw-ship-collision-exception-with-first-collision ship current-ships)
     )))

(defn deploy-ship [ship board]
  (if (are-cells-valid? (:cells ship) (:size board))
    (deploy-ship-if-no-collisions ship board)
    (throw-invalid-coordinates-exception (get-invalid-coordinates-of-ship ship (:size board)))
   ))

(defn is-cell-shot? [x-y-pair board] (contains? (:shot-cells board) x-y-pair))

(defn are-cells-shot? [cells board] (every-and-at-least-one? #(is-cell-shot? % board) cells))

(defn add-cell-to-shot-cells [x-y-pair board] (conj (:shot-cells board) x-y-pair))

(defn shoot-cell [x-y-pair board] (assoc board :shot-cells (add-cell-to-shot-cells x-y-pair board)))

(defn ship-occupies-cell? [x-y-pair ship] (contains? (:cells ship) x-y-pair))

(defn get-ship-here [x-y-pair board]
  (if (is-cell-valid? x-y-pair (:size board))
    (first (filter #(ship-occupies-cell? x-y-pair %) (:ships board)))
    (throw-invalid-coordinates-exception x-y-pair)
    )
  )

(defn is-ship-sunk? [ship board] (are-cells-shot? (:cells ship) board))

(defn is-ship-here? [x-y-pair board] (not (nil? (get-ship-here x-y-pair board))))

(defn is-ship-here-sunk? [x-y-pair board] (is-ship-sunk? (get-ship-here x-y-pair board) board))

(defn is-cell-a-hit? [x-y-pair board] (and (is-cell-shot? x-y-pair board) (is-ship-here? x-y-pair board)))

(defn are-all-ships-sunk? [board] (every-and-at-least-one? #(is-ship-sunk? % board) (:ships board)))
