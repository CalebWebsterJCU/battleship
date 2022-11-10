(ns battleship.core.cell)

(defn is-coordinate-valid? [coordinate board-size] (and (< coordinate board-size) (>= coordinate 0)))

(defn is-cell-valid? [[x y] board-size] (and (is-coordinate-valid? x board-size) (is-coordinate-valid? y board-size)))

(defn are-cells-valid? [cells board-size] (and (seq cells) (every? #(is-cell-valid? % board-size) cells)))

(defn find-invalid-cells [cells board-size] (filter #(not (is-cell-valid? % board-size)) cells))

(defn throw-invalid-coordinates-exception [x-y-pair]
  (throw (IllegalArgumentException. (str "Invalid coordinates; cell at " x-y-pair " could not be found")))
  )

(defn throw-exception-with-first-invalid-cell-found [cells board-size]
  (throw-invalid-coordinates-exception (first (find-invalid-cells cells board-size)))
  )
