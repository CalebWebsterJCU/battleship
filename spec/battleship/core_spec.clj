(ns battleship.core-spec
  (:require [speclj.core :refer :all]
            [battleship.core :refer :all]))

;simpler arguments? Only what's needed?
;invalid cell inputs? Blank?
;invalid cell input when placing ships?

(describe "Battleship"
          (context "Creating Boards"
                   (it "should be able to an create empty board"
                       (let [board (create-board 1)]
                         (should (is-board-clear? board))
                         (should= 0 (count-ships board))
                         (should-not (is-cell-shot? [0 0] board))
                         (should-be-nil (get-ship-here [0 0] board))
                         )
                       )
                   )
          (context "Placing Ships"
                   (it "should throw an exception when trying to place ship in an invalid cell"
                       (let [board (create-board 1)]
                         (should-throw (deploy-ship (create-ship "" [1 0]) board))
                         (should-throw (deploy-ship (create-ship "" [0 1]) board))
                         (should-throw (deploy-ship (create-ship "" [-1 0]) board))
                         (should-throw (deploy-ship (create-ship "" [0 -1]) board))
                         ))
                   (it "should be able to place a ship on a 1x1 board"
                       (let [board (create-board 2)
                             ship (create-ship "submarine" [0 0])
                             board-with-ship (deploy-ship ship board)]
                         (should-not (is-board-clear? board-with-ship))
                         (should= 1 (count-ships board-with-ship))
                         (should-not-be-nil (get-ship-here [0 0] board-with-ship))
                         ))
                   (it "should leave other cells empty when placing a ship on a 2x2 board"
                       (let [board (create-board 2)
                             ship (create-ship "carrier" [0 0] [1 0])
                             board-with-ship (deploy-ship ship board)]
                         (should-be-nil (get-ship-here [0 1] board-with-ship))
                         (should-be-nil (get-ship-here [1 1] board-with-ship))
                         ))
                   (it "should throw an exception when trying to place the same ship twice"
                       (let [board (create-board 1)
                             ship (create-ship "guppy" [0 0])
                             board-with-ship (deploy-ship ship board)]
                         (should-throw (deploy-ship ship board-with-ship))
                         ))
                   (it "should throw an exception when trying to place a ship on top of a different ship"
                       (let [board (create-board 2)
                             ship1 (create-ship "speedboat" [0 0] [1 0])
                             board-with-ship1 (deploy-ship ship1 board)
                             conflicting-ship (create-ship "barracuda" [0 0] [0 1])]
                         (should-throw (deploy-ship conflicting-ship board-with-ship1))
                         ))
                   )
          (context "Retrieving Ships"
                   (it "should throw an exception when trying to get a ship from an invalid cell"
                       (let [board (create-board 1)]
                         (should-throw (get-ship-here [1 0] board))
                         (should-throw (get-ship-here [0 1] board))
                         (should-throw (get-ship-here [-1 0] board))
                         (should-throw (get-ship-here [0 -1] board))
                         ))
                   (it "should not be able to retrieve ship from empty cell"
                       (let [board (create-board 1)]
                         (should-be-nil (get-ship-here [0 0] board))
                         ))
                   (it "should be able to retrieve the same ship that was placed"
                       (let [board (create-board 1)
                             ship (create-ship "pirates" [0 0])
                             board-with-ship (deploy-ship ship board)]
                         (should= ship (get-ship-here [0 0] board-with-ship))
                         ))
                   (it "should be able to retrieve a ship that occupies two cells from either cell"
                       (let [ship (create-ship "carrier" [0 0] [1 0])
                             board (deploy-ship ship (create-board 2))]
                         (should= (get-ship-here [0 0] board) (get-ship-here [1 0] board))
                         ))
                   )
          (context "Shooting Cells"
                   (it "should be able to shoot a cell on a 1x1 board"
                       (let [board (create-board 1)
                             board-with-shot (shoot-cell [0 0] board)]
                         (should-not (is-board-clear? board-with-shot))
                         (should (is-cell-shot? [0 0] board-with-shot))
                         ))
                   (it "should not shoot other cells when shooting one cell on a 2x2 board"
                       (let [board (create-board 2)
                             board-with-shot (shoot-cell [0 0] board)]
                         (should-not (is-cell-shot? [0 1] board-with-shot))
                         (should-not (is-cell-shot? [1 1] board-with-shot))
                         (should-not (is-cell-shot? [1 0] board-with-shot))
                         ))
                   (it "should not show a hit when an empty cell is shot"
                       (let [board (shoot-cell [0 0] (create-board 1))]
                         (should-not (is-cell-a-hit? [0 0] board))
                         ))
                   (it "should show a hit when a cell containing a ship is shot"
                       (let [board (create-board 1)
                             ship (create-ship "carrier" [0 0])
                             board-with-ship (deploy-ship ship board)
                             board-with-shot-ship (shoot-cell [0 0] board-with-ship)]
                         (should (is-cell-a-hit? [0 0] board-with-shot-ship))
                         ))
                   )
          (context "Classifying Sunk and Unsunk"
                   (it "should not classify a cell without a ship as sunk"
                       (let [board (create-board 1)]
                         (should-not (is-ship-here-sunk? [0 0] board))
                         ))
                   (it "should classify an un-sunk ship as un-sunk"
                       (let [ship (create-ship "submarine" [0 0])
                             board (create-board 1)
                             board-with-ship (deploy-ship ship board)]
                         (should-not (is-ship-here-sunk? [0 0] board-with-ship))
                         ))
                   (it "should classify a partially sunk ship as un-sunk"
                       (let [ship (create-ship "submarine" [0 0] [1 0])
                             board (create-board 2)
                             board-with-ship (deploy-ship ship board)
                             board-with-shot-ship (shoot-cell [0 0] board-with-ship)]
                         (should-not (is-ship-here-sunk? [0 0] board-with-shot-ship))
                         ))
                   (it "should classify a sunk ship as sunk"
                       (let [ship (create-ship "submarine" [0 0] [1 0])
                             board (create-board 2)
                             board-with-ship (deploy-ship ship board)
                             board-with-shot-ship (shoot-cell [0 0] board-with-ship)
                             board-with-sunk-ship (shoot-cell [1 0] board-with-shot-ship)]
                         (should (is-ship-here-sunk? [0 0] board-with-sunk-ship))
                         ))
                   )
          (context "Determining End of Game"
                   (it "should not be game over if 0/1 ships are sunk"
                       (let [board (create-board 1)]
                         (should-not (are-all-ships-sunk? board)))
                       )
                   (it "should be game over if 1/1 ships are sunk"
                       (let [ship (create-ship "submarine" [0 0] [1 0])
                             board (create-board 2)
                             board-with-ship (deploy-ship ship board)
                             board-with-shot-ship (shoot-cell [0 0] board-with-ship)
                             board-with-sunk-ship (shoot-cell [1 0] board-with-shot-ship)]
                         (should (are-all-ships-sunk? board-with-sunk-ship))
                         ))
                   (it "should not be game over if 1/2 ships are sunk"
                       (let [ship1 (create-ship "submarine" [0 0] [1 0])
                             ship2 (create-ship "guppy" [0 1] [1 1])
                             board (create-board 2)
                             board-with-one-ship (deploy-ship ship1 board)
                             board-with-two-ships (deploy-ship ship2 board-with-one-ship)
                             board-with-one-ship-sunk (shoot-cell [1 0] (shoot-cell [0 0] board-with-two-ships))]
                         (should-not (are-all-ships-sunk? board-with-one-ship-sunk))
                         ))
                   (it "should be game over if 2/2 ships are sunk"
                       (let [ship1 (create-ship "submarine" [0 0] [1 0])
                             ship2 (create-ship "guppy" [0 1] [1 1])
                             board (create-board 2)
                             board-with-one-ship (deploy-ship ship1 board)
                             board-with-two-ships (deploy-ship ship2 board-with-one-ship)
                             board-with-one-ship-sunk (shoot-cell [1 0] (shoot-cell [0 0] board-with-two-ships))
                             board-with-two-ships-sunk (shoot-cell [0 1] (shoot-cell [1 1] board-with-one-ship-sunk))]
                         (should (are-all-ships-sunk? board-with-two-ships-sunk))
                         ))
                   )
          )
