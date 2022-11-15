(ns battleship.game.game
  (:require [battleship.core.board :as b])
  )

(defn get-current-time [] (.atZone (java.time.Instant/now) (java.time.ZoneId/systemDefault)))

(defn create-game [player1 player2 board-size num-ships]
  {
   :player1    {:name player1 :board (b/create-board board-size)}
   :player2    {:name player2 :board (b/create-board board-size)}
   :start-time (get-current-time)
   :whose-turn :player1
   :num-ships  num-ships
   }
  )

(defn get-player [game player-key] (player-key game))

(defn get-board [game player-key] (:board (get-player game player-key)))

(defn is-players-turn? [game player-key] (= player-key (:whose-turn game)))

(defn get-opponent-key [player-key] (if (= :player1 player-key) :player2 :player1))

(defn whose-turn-next? [game] (get-opponent-key (:whose-turn game)))

(defn perform-action [game player-key action]
  (action game player-key)
  )

(defn switch-player [game]
  (assoc game :whose-turn (whose-turn-next? game))
  )

(defn create-deploy-ship-action [ship]
  (fn [game player-key]
    (assoc game player-key (assoc (get-player game player-key) :board (b/deploy-ship ship (get-board game player-key)))))
  )

(defn create-shoot-cell-action [x-y-pair]
  (fn [game player-key]
    (let [opponent-key (get-opponent-key player-key)]
      (assoc game opponent-key (assoc (get-player game opponent-key) :board (b/shoot-cell x-y-pair (get-board game opponent-key)))))
    )
  )

(defn has-player-placed-all-ships? [game player-key] (= (:num-ships game) (b/count-ships (get-board game player-key))))

(defn are-players-ships-sunk? [game player-key]
  (b/are-all-ships-sunk? (get-board game player-key))
  )

(defn have-players-placed-all-ships? [game]
  (and
    (has-player-placed-all-ships? game :player1)
    (has-player-placed-all-ships? game :player2)
    )
  )

(defn who-lost? [game]
  (cond
    (:forfeiter game) (:forfeiter game)
    (are-players-ships-sunk? game :player1) :player1
    (are-players-ships-sunk? game :player2) :player2
    :else nil
    ))

(defn is-game-over? [game] (not (nil? (who-lost? game))))

(defn set-game-end-time [game end-time]
  (assoc game :end-time end-time)
  )

(defn update-game-end-time [game]
  (if (is-game-over? game)
    (set-game-end-time game (get-current-time))
    game
    ))

(defn who-won? [game]
  (if (is-game-over? game)
    (get-opponent-key (who-lost? game))
    nil
    ))

(defn throw-not-players-turn-exception [player-name]
  (throw (RuntimeException. (str player-name " cannot perform this action; it is not their turn"))))

(defn throw-game-is-over-exception [player-name]
  (throw (RuntimeException. (str player-name " cannot perform this action; the game is over"))))

(defn throw-all-ships-not-deployed-yet-exception [player-name]
  (throw (RuntimeException. (str player-name " cannot perform this action until all ships have been deployed"))))

(defn throw-all-ships-deployed-exception [player-name]
  (throw (RuntimeException. (str player-name " cannot perform this action; all ships have been deployed"))))

(defn throw-cannot-take-turn-exception [game player-key]
  (let [player-name (:name (get-player game player-key))]
    (cond
      (is-game-over? game) (throw-game-is-over-exception player-name)
      (not (is-players-turn? game player-key)) (throw-not-players-turn-exception player-name)
      (have-players-placed-all-ships? game) (throw-all-ships-deployed-exception player-name)
      (not (have-players-placed-all-ships? game)) (throw-all-ships-not-deployed-yet-exception player-name)
      ))
  )

(defn take-players-turn [game player-key action]
  (-> game
      (perform-action player-key action)
      (switch-player)
      (update-game-end-time)
      )
  )

(defn can-player-take-turn? [game player-key]
  (and
    (not (is-game-over? game))
    (is-players-turn? game player-key)
    ))

(defn can-player-deploy-ship? [game player-key]
  (and
    (can-player-take-turn? game player-key)
    (not (have-players-placed-all-ships? game))
    ))

(defn can-player-shoot-cell? [game player-key]
  (and
    (can-player-take-turn? game player-key)
    (have-players-placed-all-ships? game)
    ))

(defn player-deploys-ship [game player-key ship]
  (let [action (create-deploy-ship-action ship)]
    (if (can-player-deploy-ship? game player-key)
      (take-players-turn game player-key action)
      (throw-cannot-take-turn-exception game player-key)
      ))
  )

(defn player-shoots-cell [game player-key x-y-pair]
  (let [action (create-shoot-cell-action x-y-pair)]
    (if (can-player-shoot-cell? game player-key)
      (take-players-turn game player-key action)
      (throw-cannot-take-turn-exception game player-key))
    ))

(defn player-forfeits-game [game player-key]
  (assoc game :forfeiter player-key)
  )

(defn get-start-time [game] (:start-time game))

(defn get-end-time [game] (:end-time game))


