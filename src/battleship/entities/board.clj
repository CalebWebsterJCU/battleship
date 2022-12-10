(ns battleship.entities.board
  (:require [battleship.entities.ship :as ship]
            [battleship.entities.cell :as cell]))

(defn has-no-ships? [board] (empty? (:ships board)))

(defn has-no-shot-cells? [board] (empty? (:shot-cells board)))

(defn is-board-clear? [board] (and (has-no-ships? board) (has-no-shot-cells? board)))

(defn create-board [size] {:size size :shot-cells #{} :ships #{}})

(defn count-ships [board] (count (:ships board)))

(defn can-ship-be-deployed? [ship board]
  (and
    (seq ship)
    (cell/are-cells-valid? ship (:size board))
    (not (ship/does-ship-collide-with-others? ship (:ships board))))
  )

(defn add-ship-to-board [ship board] (update board :ships #(conj % ship)))

(defn throw-cannot-deploy-ship-exception [ship board]
  (cond
    (= 0 (count ship)) (ship/throw-ship-has-no-cells-exception)
    (not (cell/are-cells-valid? ship (:size board))) (ship/throw-ship-has-invalid-cells-exception ship (:size board))
    (ship/does-ship-collide-with-others? ship (:ships board)) (ship/throw-ship-collides-with-others-exception ship (:ships board))
    )
  )

(defn deploy-ship [ship board]
  (if (can-ship-be-deployed? ship board)
    (add-ship-to-board ship board)
    (throw-cannot-deploy-ship-exception ship board)
    ))

(defn is-cell-shot? [x-y-pair board]
  (if (cell/is-cell-valid? x-y-pair (:size board))
    (contains? (:shot-cells board) x-y-pair)
    (cell/throw-invalid-coordinates-exception x-y-pair)
    ))

(defn are-cells-shot? [cells board] (and (seq cells) (every? #(is-cell-shot? % board) cells)))

(defn can-shoot-cell? [x-y-pair board]
  (and (not (is-cell-shot? x-y-pair board)) (cell/is-cell-valid? x-y-pair (:size board)))
  )

(defn add-shot-cell-to-board [x-y-pair board]
  (update board :shot-cells #(conj % x-y-pair))
  )

(defn throw-cannot-shoot-cell-exception [x-y-pair board]
  (cond
    (is-cell-shot? x-y-pair board) (cell/throw-cell-already-shot-exception x-y-pair)
    (not (cell/is-cell-valid? x-y-pair (:size board))) (cell/throw-invalid-coordinates-exception x-y-pair)
    )
  )

(defn shoot-cell [x-y-pair board]
  (if (can-shoot-cell? x-y-pair board)
    (add-shot-cell-to-board x-y-pair board)
    (throw-cannot-shoot-cell-exception x-y-pair board)
    )
  )

(defn get-ship-occupying-cell [x-y-pair board]
  (first (filter #(ship/ship-occupies-cell? x-y-pair %) (:ships board)))
  )

(defn get-ship-here [x-y-pair board]
  (if (cell/is-cell-valid? x-y-pair (:size board))
    (get-ship-occupying-cell x-y-pair board)
    (cell/throw-invalid-coordinates-exception x-y-pair)
    )
  )

(defn is-ship-sunk? [ship board] (are-cells-shot? ship board))

(defn is-ship-here? [x-y-pair board] (not (nil? (get-ship-here x-y-pair board))))

(defn is-ship-here-sunk? [x-y-pair board] (is-ship-sunk? (get-ship-here x-y-pair board) board))

(defn is-cell-a-hit? [x-y-pair board] (and (is-cell-shot? x-y-pair board) (is-ship-here? x-y-pair board)))

(defn are-all-ships-sunk? [board] (and (seq (:ships board)) (every? #(is-ship-sunk? % board) (:ships board))))
