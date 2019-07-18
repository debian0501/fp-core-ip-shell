
{-# LANGUAGE MultiParamTypeClasses, ForeignFunctionInterface, FlexibleInstances #-}

module Game where

import Control.Lens

type Move = (Int, Int)
type Board = [[Player]]

data Player = O | X | E deriving (Show, Eq)

data Status = Ongoing | InvalidTurn | P1_Won | P2_Won | Tied deriving (Show, Eq)

data GameState = GameState { nextPlayer :: Player
                        , board :: Board
                        , status :: Status
                        } deriving Show


initBoard :: Board
initBoard = [[E,E,E],[E,E,E],[E,E,E]]

newGame :: GameState
newGame = GameState {nextPlayer=O, board=initBoard, status=Ongoing}


game :: Move -> GameState -> GameState
game mv currGS = GameState {nextPlayer=player , board=nxtBoard, status=stat} where
       currBoard =  (board currGS)
       currPlayer = (nextPlayer currGS)
       validMove = isValidMove mv currBoard
       player = if (stat == Ongoing) then nxtPlayer currPlayer else currPlayer
       nxtBoard = if (validMove && (status currGS) == Ongoing)
                     then move currPlayer mv currBoard
                     else currBoard
       stat = nxtStatus (status currGS) currPlayer validMove nxtBoard

nxtStatus :: Status -> Player -> Bool -> Board -> Status
nxtStatus status player validMove board
        | status == Ongoing = if validMove then nxtStatusValidMove player board else InvalidTurn
        | status == InvalidTurn = if validMove then nxtStatusValidMove player board else InvalidTurn
        | otherwise = status

nxtStatusValidMove :: Player -> Board -> Status
nxtStatusValidMove player board = if (threeOfAKind player board)
                                  then playerToState player
                                  else if finishedTied board then Tied
                                  else Ongoing
playerToState :: Player -> Status
playerToState O = P1_Won
playerToState X = P2_Won

move :: Player -> Move -> Board -> Board
move player move board = case (isValidMove move board) of
         True  -> board & element (snd move) . element (fst move) .~ player
         False -> board

nxtPlayer :: Player -> Player
nxtPlayer currPlayer = case (currPlayer) of
        O -> X
        X -> O
        E -> O

isValidMove :: Move -> Board -> Bool
isValidMove move board = inRange move && stillEmpty move board where
     inRange move = (snd move) < 3 && (snd move) >= 0 && (fst move) < 3 && (fst move) >= 0
     stillEmpty move board = (board !! (snd move )) !! (fst move) == E

finishedTied :: Board -> Bool
finishedTied board = not $ head $ map (elem E) board

threeOfAKind :: Player -> Board -> Bool
threeOfAKind player board = elem (player, player, player) $ trans board

trans :: Board -> [(Player, Player, Player)]
trans board = [(a1, a2, a3), (b1, b2, b3), (c1, c2, c3), (a1,b1,c1), (a2,b2,c2), (a3,b3,c3), (a1, b2, c3), (c1, b2, a3)] where
     (a1:a2:a3:a4) = head board
     (b1:b2:b3:b4) = head (tail board)
     (c1:c2:c3:c4) = head (tail (tail board))


