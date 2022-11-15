(ns battleship.core.cell)

(defn is-coordinate-valid? [coordinate board-size] (and (< coordinate board-size) (>= coordinate 0)))

(defn is-cell-valid? [[x y] board-size] (and (is-coordinate-valid? x board-size) (is-coordinate-valid? y board-size)))

(defn are-cells-valid? [cells board-size] (every? #(is-cell-valid? % board-size) cells))

(defn find-invalid-cells [cells board-size] (filter #(not (is-cell-valid? % board-size)) cells))

(defn throw-cell-already-shot-exception [x-y-pair]
  (throw (RuntimeException. (str "Cannot shoot cell at " x-y-pair "; cell is already shot")))
  )

(defn throw-invalid-coordinates-exception [x-y-pair]
  (throw (RuntimeException. (str "Invalid coordinates; cell at " x-y-pair " does not exist"))))
