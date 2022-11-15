(ns battleship.core.board-spec
  (:require [speclj.core :refer :all]
            [battleship.core.board :refer :all]))

(describe "Battleship"
          (context "Creating Boards:"
                   (it "should be able to an create empty board"
                       (let [board (create-board 1)]
                         (should (is-board-clear? board))
                         (should= 0 (count-ships board))
                         ))
                   )
          (context "Deploying Ships:"
                   (it "should throw an exception when trying to deploy ship in an invalid cell"
                       (let [board (create-board 1)]
                         (should-throw RuntimeException
                                       (deploy-ship #{[0 0] [1 0] [0 1]} board))
                         (should-throw RuntimeException
                                       (deploy-ship #{[0 0] [0 -1] [-1 0]} board))
                         ))
                   (it "should throw an exception when trying to deploy ship with no cells"
                       (let [board (create-board 1)]
                         (should-throw RuntimeException
                                       (deploy-ship #{} board))
                         ))
                   (it "should be able to deploy a ship on a 1x1 board"
                       (let [board (->> (create-board 1)
                                        (deploy-ship #{[0 0]}))]
                         (should-not (is-board-clear? board))
                         (should= 1 (count-ships board))
                         (should-not-be-nil (get-ship-here [0 0] board))
                         ))
                   (it "should leave other cells empty when deploying a ship on a 2x2 board"
                       (let [board (->> (create-board 2)
                                        (deploy-ship #{[0 0] [1 0]}))]
                         (should-be-nil (get-ship-here [0 1] board))
                         (should-be-nil (get-ship-here [1 1] board))
                         ))
                   (it "should throw an exception when trying to deploy the same ship twice"
                       (let [board (->> (create-board 1)
                                        (deploy-ship #{[0 0]}))]
                         (should-throw RuntimeException
                                       (deploy-ship #{[0 0]} board))
                         ))
                   (it "should throw an exception when trying to deploy a ship on top of a different ship"
                       (let [board (create-board 2)
                             board-with-ship1 (deploy-ship #{[0 0] [1 0]} board)]
                         (should-throw (deploy-ship #{[0 0] [0 1]} board-with-ship1))
                         ))
                   )
          (context "Retrieving Ships:"
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
                       (let [board (->> (create-board 1)
                                        (deploy-ship #{[0 0]}))]
                         (should= #{[0 0]} (get-ship-here [0 0] board))
                         ))
                   (it "should be able to retrieve a ship that occupies two cells from either cell"
                       (let [board (->> (create-board 2)
                                        (deploy-ship #{[0 0] [1 0]}))]
                         (should= (get-ship-here [0 0] board) (get-ship-here [1 0] board))
                         ))
                   )
          (context "Shooting Cells:"
                   (it "should throw an exception when trying to shoot an invalid cell"
                       (let [board (create-board 1)]
                         (should-throw (shoot-cell [0 1] board))
                         (should-throw (shoot-cell [-1 0] board))
                         (should-throw (shoot-cell [1 0] board))
                         (should-throw (shoot-cell [0 -1] board))
                         ))
                   (it "should throw an exception when asking if an invalid cell is shot"
                       (let [board (create-board 1)]
                         (should-throw (is-cell-shot? [0 1] board))
                         (should-throw (is-cell-shot? [-1 0] board))
                         (should-throw (is-cell-shot? [1 0] board))
                         (should-throw (is-cell-shot? [0 -1] board))
                         ))
                   (it "should be able to shoot a cell on a 1x1 board"
                       (let [board (->> (create-board 1)
                                        (shoot-cell [0 0]))]
                         (should-not (is-board-clear? board))
                         (should (is-cell-shot? [0 0] board))
                         ))
                   (it "should not shoot other cells when shooting one cell on a 2x2 board"
                       (let [board (->> (create-board 2)
                                        (shoot-cell [0 0]))]
                         (should-not (is-cell-shot? [0 1] board))
                         (should-not (is-cell-shot? [1 1] board))
                         (should-not (is-cell-shot? [1 0] board))
                         ))
                   (it "should not show a hit when an empty cell is shot"
                       (let [board (shoot-cell [0 0] (create-board 1))]
                         (should-not (is-cell-a-hit? [0 0] board))
                         ))
                   (it "should show a hit when a cell containing a ship is shot"
                       (let [board (->> (create-board 1)
                                        (deploy-ship #{[0 0]})
                                        (shoot-cell [0 0]))]
                         (should (is-cell-a-hit? [0 0] board))
                         ))
                   (it "should throw an exception when a cell is shot twice"
                       (let [board (->> (create-board 1)
                                        (shoot-cell [0 0]))]
                         (should-throw RuntimeException
                                       (shoot-cell [0 0] board))
                         ))
                   )
          (context "Classifying Sunk and Unsunk:"
                   (it "should not classify a cell without a ship as sunk"
                       (let [board (create-board 1)]
                         (should-not (is-ship-here-sunk? [0 0] board))
                         ))
                   (it "should classify an un-sunk ship as un-sunk"
                       (let [board (->> (create-board 1)
                                        (deploy-ship #{[0 0]}))]
                         (should-not (is-ship-here-sunk? [0 0] board))
                         ))
                   (it "should classify a partially sunk ship as un-sunk"
                       (let [board (->> (create-board 2)
                                        (deploy-ship #{[0 0] [1 0]})
                                        (shoot-cell [0 0]))]
                         (should-not (is-ship-here-sunk? [0 0] board))
                         ))
                   (it "should classify a sunk ship as sunk"
                       (let [board (->> (create-board 2)
                                        (deploy-ship #{[0 0] [1 0]})
                                        (shoot-cell [0 0])
                                        (shoot-cell [1 0]))]
                         (should (is-ship-here-sunk? [0 0] board))
                         ))
                   )
          (context "Determining End of Game:"
                   (it "should not be game over if 0/1 ships are sunk"
                       (let [board (create-board 1)]
                         (should-not (are-all-ships-sunk? board)))
                       )
                   (it "should not be game over if 1/2 ships are sunk"
                       (let [board (->> (create-board 2)
                                        (deploy-ship #{[0 0] [1 0]})
                                        (deploy-ship #{[0 1] [1 1]})
                                        (shoot-cell [0 0])
                                        (shoot-cell [1 0]))]
                         (should-not (are-all-ships-sunk? board))
                         ))
                   (it "should be game over if 2/2 ships are sunk"
                       (let [board (->> (create-board 2)
                                        (deploy-ship #{[0 0] [1 0]})
                                        (deploy-ship #{[0 1] [1 1]})
                                        (shoot-cell [0 0])
                                        (shoot-cell [1 0])
                                        (shoot-cell [0 1])
                                        (shoot-cell [1 1]))]
                         (should (are-all-ships-sunk? board))
                         ))
                   )
          )
