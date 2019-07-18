{-# LANGUAGE MultiParamTypeClasses, ForeignFunctionInterface, FlexibleInstances #-}

module GameFFI where

import Java
import Game
import Control.Monad

-- foreign export
foreign export java "@static de.maibornwolff.tictactoecore.business.Game.newGame" newJGame :: JGameState
foreign export java "@static de.maibornwolff.tictactoecore.business.Game.game" jGame :: JMove -> JGameState


-- GameState
data JGameState = JGameState @de.maibornwolff.tictactoeshell.transport.GameState
  deriving Class



-- foreign import

foreign import java unsafe "@new" newJGameState  :: JIntIntArray -> JStatus -> JPlayer -> JGameState

foreign import java unsafe "getNextPlayer" getNextPlayer  :: JGameState -> JPlayer

foreign import java unsafe "getStatus" getStatus  :: JGameState -> JStatus

foreign import java unsafe "getBoard" getBoard  :: JGameState -> JIntIntArray


-- Player
data JPlayer = JPlayer @de.maibornwolff.tictactoeshell.transport.Player
  deriving (Class, Eq)

foreign import java unsafe
  "@static @field de.maibornwolff.tictactoeshell.transport.Player.P1"
  player1 :: JPlayer

foreign import java unsafe
  "@static @field de.maibornwolff.tictactoeshell.transport.Player.P2"
  player2 :: JPlayer

--- Status
data JStatus = JStatus @de.maibornwolff.tictactoeshell.transport.Status
  deriving Class

foreign import java unsafe
  "@static @field de.maibornwolff.tictactoeshell.transport.Status.ONGOING"
  ongoing :: JStatus

foreign import java unsafe
  "@static @field de.maibornwolff.tictactoeshell.transport.Status.INVALID_TURN"
  invalidTurn :: JStatus

foreign import java unsafe
  "@static @field de.maibornwolff.tictactoeshell.transport.Status.TIED"
  tied :: JStatus

foreign import java unsafe
  "@static @field de.maibornwolff.tictactoeshell.transport.Status.P1_WON"
  p1Won :: JStatus

foreign import java unsafe
  "@static @field de.maibornwolff.tictactoeshell.transport.Status.P2_WON"
  p2Won :: JStatus

-- Move
data JMove = JMove @de.maibornwolff.tictactoeshell.transport.Move
  deriving Class

foreign import java unsafe "getField" getField  :: JMove -> JField

foreign import java unsafe "getGameState" getGameState  :: JMove -> JGameState

data JField = JField @de.maibornwolff.tictactoeshell.transport.Field
  deriving Class

foreign import java unsafe "getX" getX  :: JField -> Int

foreign import java unsafe "getY" getY  :: JField -> Int


data JIntIntArray = JIntIntArray @int[][]
    deriving Class

instance JArray JIntArray JIntIntArray

-- foreign
instance JavaConverter GameState JGameState where
    toJava   = toJGameState
    fromJava = fromJGameState

newJGame :: JGameState
newJGame = toJava newGame

toJGameState :: GameState -> JGameState
toJGameState gs = newJGameState b st player where
    b = toJIntIntArray (board gs)
    player = case (nextPlayer gs) of
        X -> player1
        O -> player2
    st = case (status gs) of
        Ongoing -> ongoing
        InvalidTurn -> invalidTurn
        P1_Won -> p1Won
        P2_Won -> p2Won
        Tied -> tied

fromJGameState :: JGameState -> GameState
fromJGameState jgs = GameState {nextPlayer= player, board=b, status=st} where
    b = fromJIntIntArray $ getBoard jgs
    player = toNPlayer (getNextPlayer jgs)
    st = case (getStatus jgs) of
         ongoing -> Ongoing

toNPlayer :: JPlayer -> Player
toNPlayer p = if p == player1
                then  X
                else if p == player2
                    then O
                else E


jGame :: JMove -> JGameState
jGame move = toJGameState $ game (toMove move) (fromJGameState $ getGameState move)

toMove :: JMove -> Move
toMove move = (getX $ getField move, getY $ getField move)

toInt :: Player -> Int
toInt x = if x == X then 1 else (if x == O then 2 else 0)

toPlayer :: Int -> Player
toPlayer x = if x == 1 then X else (if x == 2 then O else E)


toJIntIntArray :: [[Player]] -> JIntIntArray
toJIntIntArray board = jints $ map (map (toInt)) board

fromJIntIntArray :: JIntIntArray -> [[Player]]
fromJIntIntArray board = map (map (toPlayer)) $ ints board


jints :: [[Int]] -> JIntIntArray
jints intss = unsafePerformJava $ do
    arr <- anew (length intss)
    forM_ (zip [0..] intss) $ \(i, ints) ->
        arr Java.<.> aset i (toJava ints)
    return arr

ints :: JIntIntArray -> [[Int]]
ints iiarr = unsafePerformJava $ do
    iarrs <- iiarr Java.<.> arrayToList
    forM iarrs $ \iarr -> iarr Java.<.> arrayToList

-- bridge