(ns battleship.game.game-spec
  (:require [speclj.core :refer :all]
            [battleship.game.game :refer :all]
            [battleship.core.board :as b]
            ))

(describe "Battleship Game Interface"
          (it "should be able to create a new game"
              (let [game (start-game 1)
                    p1-board (:board1 game)
                    p2-board (:board2 game)]
                (should= 1 (:size p1-board))
                (should= 1 (:size p2-board))
                (should-not-be-nil p1-board)
                (should-not-be-nil p2-board)
                (should (b/is-board-clear? p1-board))
                (should (b/is-board-clear? p2-board))
                ))
          )