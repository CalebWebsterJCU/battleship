(ns battleship.entities.game-spec
  (:require [speclj.core :refer :all]
            [battleship.entities.game :as game]
            ))

(describe "Battleship Game"
          (it "should be able to start a new game"
              (let [game (game/create-game 1 1)]
                (should-not (game/is-game-over? game))
                (should-be-nil (game/who-won? game))
                ))
          (it "should not allow player 2 to take the first turn"
              (let [game (game/create-game 1 1)]
                (should-throw RuntimeException
                              (game/player-deploys-ship game :player2 #{[0 0]}))
                ))
          (it "should not allow a player to take a turn twice in a row"
              (let [game (-> (game/create-game 1 1)
                             (game/player-deploys-ship :player1 #{[0 0]})
                             (game/player-deploys-ship :player2 #{[0 0]}))]
                (should-throw RuntimeException
                              (game/player-shoots-cell game :player2 [0 0]))
                ))
          (it "should not allow players to place ships during attack phase"
              (let [game (-> (game/create-game 2 1)
                             (game/player-deploys-ship :player1 #{[0 0]})
                             (game/player-deploys-ship :player2 #{[0 0]}))]
                (should-throw RuntimeException
                              (game/player-deploys-ship game :player1 #{[1 0]}))
                ))
          (it "should not allow players to shoot cells during planning phase"
              (let [game (game/create-game 1 1)]
                (should-throw RuntimeException
                              (game/player-shoots-cell game :player1 [0 0]))
                ))
          (it "should end the game and store the winner when a player sinks all the other player's ships"
              (let [game (-> (game/create-game 2 2)
                             (game/player-deploys-ship :player1 #{[0 0]})
                             (game/player-deploys-ship :player2 #{[0 0]})
                             (game/player-deploys-ship :player1 #{[1 0]})
                             (game/player-deploys-ship :player2 #{[1 0]})
                             (game/player-shoots-cell :player1 [0 0])
                             (game/player-shoots-cell :player2 [0 0])
                             (game/player-shoots-cell :player1 [1 0]))]
                (should= :player1 (game/who-won? game))
                ))
          (it "should not allow players to take turns when the game is over"
              (let [game (-> (game/create-game 2 2)
                             (game/player-deploys-ship :player1 #{[0 0]})
                             (game/player-deploys-ship :player2 #{[0 0]})
                             (game/player-deploys-ship :player1 #{[1 0]})
                             (game/player-deploys-ship :player2 #{[1 0]})
                             (game/player-shoots-cell :player1 [0 0])
                             (game/player-shoots-cell :player2 [0 0])
                             (game/player-shoots-cell :player1 [1 0]))]
                (should-throw RuntimeException
                              (game/player-shoots-cell game :player2 [1 0]))
                ))
          (it "should end the game and store the winner when a player forfeits"
              (let [game (-> (game/create-game 1 1)
                             (game/player-forfeits-game :player1))]
                (should= :player2 (game/who-won? game))
                ))
          )