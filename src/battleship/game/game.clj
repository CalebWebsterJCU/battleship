(ns battleship.game.game
  (:require [battleship.core.board :as b])
  )

(defn create-game [player1 player2 board-size num-ships]
  {
   :player1    {:name player1 :board (b/create-board board-size)}
   :player2    {:name player2 :board (b/create-board board-size)}
   :start-time (java.time.Instant/now)
   :phase      :planning
   :whose-turn :player1
   :num-ships  num-ships
   }
  )

(defn get-player [game player-key] (player-key game))

(defn get-board [game player-key] (:board (get-player game player-key)))

(defn is-players-turn? [game player-key] (= player-key (:whose-turn game)))

(defn get-opponent-key [player-key] (if (= :player1 player-key) :player2 :player1))

(defn get-opponent [game player-key] (get-player game (get-opponent-key player-key)))

(defn get-opponent-board [game player-key] (:board (get-opponent game player-key)))

(defn whose-turn-next? [game] (if (is-players-turn? game :player1) :player2 :player1))

(defn throw-not-players-turn-exception [game player-key] (throw (RuntimeException. (str "Cannot perform action; it is not " (:name (player-key game)) "'s turn"))))

(defn throw-cannot-place-ship-during-attack-phase-exception [] (throw (RuntimeException. "Cannot place ship during attack phase")))

(defn throw-cannot-shoot-cell-during-planning-phase-exception [] (throw (RuntimeException. "Cannot shoot cell during planning phase")))

(defn throw-game-is-over-exception [] (throw (RuntimeException. "Cannot perform action; the game is over")))

(defn perform-players-action [game player-key action]
  (action game player-key)
  )

(defn change-player [game]
  (assoc game :whose-turn (whose-turn-next? game))
  )

(defn get-winner [game]
  (if (b/are-all-ships-sunk? (get-board game :player1))
    :player2
    (if (b/are-all-ships-sunk? (get-board game :player2))
      :player1
      nil
      )
    ))

(defn is-game-over? [game] (not (nil? (get-winner game))))

(defn perform-players-action-if-players-turn [game player-key action]
  (if (is-players-turn? game player-key)
    (perform-players-action game player-key action)
    (throw-not-players-turn-exception game player-key))
  )

(defn perform-players-action-if-game-not-over [game player-key action]
  (if (not (is-game-over? game))
    (perform-players-action-if-players-turn game player-key action)
    (throw-game-is-over-exception)
    ))

(defn perform-players-action-and-change-player [game player-key action]
  (change-player (perform-players-action-if-game-not-over game player-key action))
  )

(defn create-ship-deploy-action [ship]
  (fn [game player-key] (assoc game player-key (assoc (get-player game player-key) :board (b/deploy-ship ship (get-board game player-key)))))
  )

(defn create-shoot-cell-action [x-y-pair]
  (fn [game player-key] (assoc game (get-opponent-key player-key) (assoc (get-opponent game player-key) :board (b/shoot-cell x-y-pair (get-opponent-board game player-key)))))
  )

(defn has-player-placed-all-ships? [game player-key] (= (:num-ships game) (b/count-ships (get-board game player-key))))

(defn is-planning-phase-over? [game] (and (is-players-turn? game :player1) (has-player-placed-all-ships? game :player1)))

(defn is-in-attack-phase? [game] (= :attack (:phase game)))

(defn is-in-planning-phase? [game] (= :planning (:phase game)))

(defn switch-to-attack-phase [game] (assoc game :phase :attack))

(defn set-winner [game winner] (assoc game :winner winner))

(defn update-game-phase [game]
  (if (is-planning-phase-over? game)
    (switch-to-attack-phase game)
    game
    ))

(defn update-game-state [game]
  (if (is-game-over? game)
    (set-winner game (get-winner game))
    (update-game-phase game)
    ))

(defn perform-players-action-and-update-game [game player-key action]
  (update-game-state (perform-players-action-and-change-player game player-key action))
  )

(defn player-deploys-ship [game player-key ship]
  (let [action (create-ship-deploy-action ship)]
    (if (is-in-planning-phase? game)
      (perform-players-action-and-update-game game player-key action)
      (throw-cannot-place-ship-during-attack-phase-exception)
      ))
  )

(defn player-shoots-cell [game player-key x-y-pair]
  (let [action (create-shoot-cell-action x-y-pair)]
    (if (is-in-attack-phase? game)
      (perform-players-action-and-update-game game player-key action)
      (throw-cannot-shoot-cell-during-planning-phase-exception)
      )))

(defn get-start-year [game] (.getYear (.atZone (:start-time game) (java.time.ZoneId/systemDefault))))
(defn get-start-month [game] (.getMonth (.atZone (:start-time game) (java.time.ZoneId/systemDefault))))
(defn get-start-day [game] (.getDayOfWeek (.atZone (:start-time game) (java.time.ZoneId/systemDefault))))
(defn get-start-hour [game] (.getHour (.atZone (:start-time game) (java.time.ZoneId/systemDefault))))
(defn get-start-minute [game] (.getMinute (.atZone (:start-time game) (java.time.ZoneId/systemDefault))))
(defn get-start-second [game] (.getSecond (.atZone (:start-time game) (java.time.ZoneId/systemDefault))))

