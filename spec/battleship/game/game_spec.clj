(ns battleship.game.game-spec
  (:require [speclj.core :refer :all]
            [battleship.game.game :refer :all]
            [battleship.core.board :as b]
            ))

;different time zones?
;complete stage?
;winning the game
;cannot keep playing if game is won
;forfeit the game?

(defn get-current-year [] (.getYear (.atZone (java.time.Instant/now) (java.time.ZoneId/systemDefault))))
(defn get-current-month [] (.getMonth (.atZone (java.time.Instant/now) (java.time.ZoneId/systemDefault))))
(defn get-current-day [] (.getDayOfWeek (.atZone (java.time.Instant/now) (java.time.ZoneId/systemDefault))))
(defn get-current-hour [] (.getHour (.atZone (java.time.Instant/now) (java.time.ZoneId/systemDefault))))
(defn get-current-minute [] (.getMinute (.atZone (java.time.Instant/now) (java.time.ZoneId/systemDefault))))
(defn get-current-second [] (.getSecond (.atZone (java.time.Instant/now) (java.time.ZoneId/systemDefault))))

(describe "Battleship Game"
          (it "should be able to start a new game"
              (let [game (create-game "p1" "p2" 1 1)
                    board1 (:board (:player1 game))
                    board2 (:board (:player2 game))]
                (should= "p1" (:name (:player1 game)))
                (should= "p2" (:name (:player2 game)))
                (should-be-nil (:winner game))
                (should= 1 (:size board1))
                (should= 1 (:size board2))
                (should= :planning (:phase game))
                (should= :player1 (:whose-turn game))
                (should= 1 (:num-ships game))
                (should-be-nil (:winner game))
                (should (b/is-board-clear? board1))
                (should (b/is-board-clear? board2))
                (should= (get-current-year) (get-start-year game))
                (should= (get-current-month) (get-start-month game))
                (should= (get-current-day) (get-start-day game))
                (should= (get-current-hour) (get-start-hour game))
                (should= (get-current-minute) (get-start-minute game))
                (should= (get-current-second) (get-start-second game))
                (should-be-nil (:end-time game))
                ))
          (it "should allow both players to place ships in turns"
              (let [game (-> (create-game "p1" "p2" 2 2)
                             (player-deploys-ship :player1 #{[0 0]})
                             (player-deploys-ship :player2 #{[0 0]})
                             (player-deploys-ship :player1 #{[0 1]})
                             (player-deploys-ship :player2 #{[0 1]}))]
                (should (b/is-ship-here? [0 0] (get-board game :player1)))
                (should (b/is-ship-here? [0 0] (get-board game :player2)))
                (should (b/is-ship-here? [0 1] (get-board game :player1)))
                (should (b/is-ship-here? [0 1] (get-board game :player2)))
                ))
          (it "should not allow player2 to take the first turn"
              (let [game (create-game "p1" "p2" 1 1)]
                (should-throw RuntimeException "Cannot perform action; it is not p2's turn"
                              (player-deploys-ship game :player2 #{[0 0]}))
                ))
          (it "should not allow a player to take a turn twice in a row"
              (let [game (-> (create-game "p1" "p2" 1 1)
                             (player-deploys-ship :player1 #{[0 0]})
                             (player-deploys-ship :player2 #{[0 0]})
                             )]
                (should-throw RuntimeException "Cannot perform action; it is not p2's turn"
                              (player-shoots-cell game :player2 [0 0]))
                ))
          (it "should move to attack stage when enough ships are placed"
              (let [game (-> (create-game "p1" "p2" 1 1)
                             (player-deploys-ship :player1 #{[0 0]})
                             (player-deploys-ship :player2 #{[0 0]}))]
                (should= :attack (:phase game))
                ))
          (it "should not allow players to place ships during attack phase"
              (let [game (-> (create-game "p1" "p2" 2 1)
                             (player-deploys-ship :player1 #{[0 0]})
                             (player-deploys-ship :player2 #{[0 0]}))]
                (should-throw RuntimeException "Cannot place ship during attack phase"
                              (player-deploys-ship game :player1 #{[1 0]}))
                ))
          (it "should allow both players to shoot cells in turns"
              (let [game (-> (create-game "p1" "p2" 2 1)
                             (player-deploys-ship :player1 #{[0 0] [1 0]})
                             (player-deploys-ship :player2 #{[0 1] [1 1]})
                             (player-shoots-cell :player1 [0 0])
                             (player-shoots-cell :player2 [0 1])
                             (player-shoots-cell :player1 [1 0])
                             (player-shoots-cell :player2 [1 1]))]
                (should (b/is-cell-shot? [0 0] (get-board game :player2)))
                (should (b/is-cell-shot? [0 1] (get-board game :player1)))
                (should (b/is-cell-shot? [1 0] (get-board game :player2)))
                (should (b/is-cell-shot? [1 1] (get-board game :player1)))
                ))
          (it "should not allow players to shoot cells during planning phase"
              (let [game (create-game "p1" "p2" 1 1)]
                (should-throw RuntimeException "Cannot shoot cell during planning phase"
                              (player-shoots-cell game :player1 [0 0]))
                ))
          (it "should store the winner when the game is over"
              (let [game (-> (create-game "p1" "p2" 2 2)
                             (player-deploys-ship :player1 #{[0 0]})
                             (player-deploys-ship :player2 #{[0 0]})
                             (player-deploys-ship :player1 #{[1 0]})
                             (player-deploys-ship :player2 #{[1 0]})
                             (player-shoots-cell :player1 [0 0])
                             (player-shoots-cell :player2 [0 0])
                             (player-shoots-cell :player1 [1 0]))]
                (should (b/are-all-ships-sunk? (get-board game :player2)))
                (should= :player1 (get-winner game))
                ))
          (it "should not allow players to take turns when the game is over"
              (let [game (-> (create-game "p1" "p2" 2 2)
                             (player-deploys-ship :player1 #{[0 0]})
                             (player-deploys-ship :player2 #{[0 0]})
                             (player-deploys-ship :player1 #{[1 0]})
                             (player-deploys-ship :player2 #{[1 0]})
                             (player-shoots-cell :player1 [0 0])
                             (player-shoots-cell :player2 [0 0])
                             (player-shoots-cell :player1 [1 0]))]
                (should-throw RuntimeException "Cannot perform action; the game is over"
                              (player-shoots-cell game :player2 [1 0]))
              ))
          )