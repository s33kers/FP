module Game where

import Models
import Connection
import Tactics
import Data.Char
import Data.List
import Control.Monad

play :: Setup -> IO ()
play setup = do
  when (mode == Attack) $ void $ sendMoves gId pId [firstMove playerSymbol]
  startMoves gId pId playerSymbol True
    where
      mode = gameMode setup
      gId = gameId setup
      pId = playerId setup
      playerSymbol = symbol setup

startMoves :: String -> String -> Char -> Bool -> IO ()
startMoves gameID playerID playerSymbol True = do
  moves <- getMoves playerID playerID
  makeNextMove gameID playerID playerSymbol moves
startMoves _ _ _ False = print $ "The end."

makeNextMove :: String -> String -> Char -> Moves -> IO ()
makeNextMove gId pId playerSymbol moves
  | currentBoardSize < 9 && (not $ winnerExists moves) = do
    moveToTry <- nextMove playerSymbol moves
    case moveToTry of
        Just move -> void $ sendMoves gId pId (moves ++ [move])
        Nothing -> do
            print $ "Do not know where to move!"
            startMoves gId pId playerSymbol False
    if currentBoardSize /= 8
        then startMoves gId pId playerSymbol True
        else startMoves gId pId playerSymbol False
  | otherwise = startMoves gId pId playerSymbol False
  where
    currentBoardSize = length moves
