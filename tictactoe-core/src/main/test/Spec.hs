-- test/LibTests.hs
module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Lib

gameBoard :: Board
gameBoard = [[O,E,O], [E, E, E], [X, E, X]]

gameBoardTied :: Board
gameBoardTied = [[O,O,X], [X, E, O], [O, X, X]]

gameBoardFinished :: Board
gameBoardFinished = [[O,O,O], [E,E,E], [X,E,X]]

gs1 :: GameState
gs1 = GameState {nextPlayer=O, board=gameBoard, status=Ongoing}

gs2 :: GameState
gs2 = GameState {nextPlayer=X, board=gameBoard, status=Ongoing} 

gs3 :: GameState
gs3 = GameState {nextPlayer=O, board=gameBoardTied, status=Ongoing} 

gs4 :: GameState
gs4 = GameState {nextPlayer=X, board=gameBoardFinished, status=P1_Won}

gs5 :: GameState
gs5 = GameState {nextPlayer=O, board=gameBoard, status=InvalidTurn}

main :: IO ()
main = do
  defaultMain (testGroup "Our Library Tests" [moveOutsideGameFieldTest
                                             ,moveInsideGameFieldTest
                                             ,moveOnAlreadyUsedFieldTest
                                             ,gameInvalidTurnTest
                                             ,gameFinishedP1Test
                                             ,gameFinishedP2Test
                                             ,gameFinishedTiedTest
                                             ,moveAfterGameFinishedTest
                                             ,moveAfterInvalidMoveTest ])

moveOutsideGameFieldTest :: TestTree
moveOutsideGameFieldTest = testCase "Move outside game field"
    (assertBool "Move should not be valid" (False == (isValidMove (0,3) initBoard)))

moveInsideGameFieldTest :: TestTree
moveInsideGameFieldTest = testCase "Move inside game field"
    (assertBool "Move should be valid" (True == (isValidMove (1,1) initBoard)))

moveOnAlreadyUsedFieldTest :: TestTree
moveOnAlreadyUsedFieldTest = testCase "Move on already used game field"
    (assertBool "Move should be invalid" (False == (isValidMove (0,0) gameBoard))) 

gameInvalidTurnTest :: TestTree
gameInvalidTurnTest = testCase "Move is a invalid Turn" $ do
    assertEqual "The turn must be invalid because the field is already used" InvalidTurn (status (game (0,0) gs1))
    assertEqual "The next player must still be P1 because of an invalid move" O (nextPlayer (game (0,0) gs1))

gameFinishedP1Test :: TestTree 
gameFinishedP1Test = testCase "Move finishes game with P1_Won"
    (assertEqual "The winner should be P1" P1_Won (status (game (1,0) gs1)))

gameFinishedP2Test :: TestTree 
gameFinishedP2Test = testCase "Move finishes game with P2_Won"
    (assertEqual "The winner should be P2" P2_Won (status (game (1,2) gs2)))

gameFinishedTiedTest :: TestTree 
gameFinishedTiedTest = testCase "Move finishes game with Tied"
    (assertEqual "The should finish tied" Tied (status (game (1,1) gs3)))

moveAfterGameFinishedTest :: TestTree
moveAfterGameFinishedTest = testCase "Move after finished game" $ do
    assertEqual "There should be no change on the status of a finished game" P1_Won (status (game (1,2) gs4)) 
    assertEqual "There should be no change on the board of a finished game" gameBoardFinished  (board (game (1,2) gs4)) 
    assertEqual "The player should not change on a finished game" X (nextPlayer (game(1,2) gs4))

moveAfterInvalidMoveTest :: TestTree
moveAfterInvalidMoveTest = testCase "Move after invalid move" $ do
    assertEqual "The status should not be InvalidTurn anymore" Ongoing (status (game (1,1) gs5))
    assertEqual "The next player should not be X" X (nextPlayer (game (1,1) gs5))

