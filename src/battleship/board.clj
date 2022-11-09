(ns battleship.board
  (:require [battleship.ship :as ship]
            [battleship.cell :as cell]))

(defn has-no-ships? [board] (empty? (:ships board)))

(defn has-no-shot-cells? [board] (empty? (:shot-cells board)))

(defn is-board-clear? [board] (and (has-no-ships? board) (has-no-shot-cells? board)))

(defn create-board [size] {:size size :shot-cells #{} :ships #{}})

(defn count-ships [board] (count (:ships board)))

(defn deploy-ship-if-no-collisions [ship board]
  (let [current-ships (:ships board)]
    (if (not (ship/does-ship-collide-with-others? ship current-ships))
      (assoc board :ships (conj current-ships ship))
      (ship/throw-exception-with-first-collision-found ship current-ships)
      )))

(defn deploy-ship [ship board]
  (let [board-size (:size board)]
   (if (cell/are-cells-valid? ship board-size)
     (deploy-ship-if-no-collisions ship board)
     (cell/throw-exception-with-first-invalid-cell-found ship board-size)
     )))

(defn is-cell-shot? [x-y-pair board]
  (if (cell/is-cell-valid? x-y-pair (:size board))
    (contains? (:shot-cells board) x-y-pair)
    (cell/throw-invalid-coordinates-exception x-y-pair)
    ))

(defn are-cells-shot? [cells board] (and (seq cells) (every? #(is-cell-shot? % board) cells)))

(defn shoot-cell [x-y-pair board]
  (if (cell/is-cell-valid? x-y-pair (:size board))
    (assoc board :shot-cells (conj (:shot-cells board) x-y-pair))
    (cell/throw-invalid-coordinates-exception x-y-pair)
    ))

(defn get-ship-here [x-y-pair board]
  (if (cell/is-cell-valid? x-y-pair (:size board))
    (first (filter #(ship/ship-occupies-cell? x-y-pair %) (:ships board)))
    (cell/throw-invalid-coordinates-exception x-y-pair)
    )
  )

(defn is-ship-sunk? [ship board] (are-cells-shot? ship board))

(defn is-ship-here? [x-y-pair board] (not (nil? (get-ship-here x-y-pair board))))

(defn is-ship-here-sunk? [x-y-pair board] (is-ship-sunk? (get-ship-here x-y-pair board) board))

(defn is-cell-a-hit? [x-y-pair board] (and (is-cell-shot? x-y-pair board) (is-ship-here? x-y-pair board)))

(defn are-all-ships-sunk? [board] (and (seq (:ships board)) (every? #(is-ship-sunk? % board) (:ships board))))
