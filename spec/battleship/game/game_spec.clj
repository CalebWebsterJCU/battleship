(ns battleship.game.game-spec
  (:require [speclj.core :refer :all]
            [battleship.game.game :refer :all]
            ))

;different time zones?

(describe "Battleship Game"
          (it "should be able to start a new game"
              (let [game (create-game "p1" "p2" 1 1)]
                (should-not (is-game-over? game))
                (should (is-players-turn? game :player1))
                (should (not (have-players-placed-all-ships? game)))
                ))
          (it "should not allow player2 to take the first turn"
              (let [game (create-game "p1" "p2" 1 1)]
                (should-throw RuntimeException
                              (player-deploys-ship game :player2 #{[0 0]}))
                ))
          (it "should not allow a player to take a turn twice in a row"
              (let [game (-> (create-game "p1" "p2" 1 1)
                             (player-deploys-ship :player1 #{[0 0]})
                             (player-deploys-ship :player2 #{[0 0]})
                             )]
                (should-throw RuntimeException
                              (player-shoots-cell game :player2 [0 0]))
                ))
          (it "should move to attack stage when enough ships are placed"
              (let [game (-> (create-game "p1" "p2" 1 1)
                             (player-deploys-ship :player1 #{[0 0]})
                             (player-deploys-ship :player2 #{[0 0]}))]
                (should (have-players-placed-all-ships? game))
                ))
          (it "should not allow players to place ships during attack phase"
              (let [game (-> (create-game "p1" "p2" 2 1)
                             (player-deploys-ship :player1 #{[0 0]})
                             (player-deploys-ship :player2 #{[0 0]}))]
                (should-throw RuntimeException
                              (player-deploys-ship game :player1 #{[1 0]}))
                ))
          (it "should not allow players to shoot cells during planning phase"
              (let [game (create-game "p1" "p2" 1 1)]
                (should-throw RuntimeException
                              (player-shoots-cell game :player1 [0 0]))
                ))
          (it "should be able to provide the winner game ends"
              (let [game (-> (create-game "p1" "p2" 2 2)
                             (player-deploys-ship :player1 #{[0 0]})
                             (player-deploys-ship :player2 #{[0 0]})
                             (player-deploys-ship :player1 #{[1 0]})
                             (player-deploys-ship :player2 #{[1 0]})
                             (player-shoots-cell :player1 [0 0])
                             (player-shoots-cell :player2 [0 0])
                             (player-shoots-cell :player1 [1 0]))]
                (should= :player1 (who-won? game))
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
                (should-throw RuntimeException
                              (player-shoots-cell game :player2 [1 0]))
                ))
          (it "should end the game and store the winner when a player forfeits"
              (let [game (-> (create-game "p1" "p2" 1 1)
                             (player-forfeits-game :player1))]
                (should= :player2 (who-won? game))
                ))
          )