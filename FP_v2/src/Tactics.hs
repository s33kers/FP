module Tactics where

import Models
import Data.Char
import Data.List
import Control.Monad
import System.Random
import qualified Data.Set   as Set

opponent :: Char -> Char
opponent 'x' = 'o'
opponent 'o' = 'x'

inverseMovePlayer :: Move -> Move
inverseMovePlayer (Move x y 'x') = Move x y 'o'
inverseMovePlayer (Move x y 'o') = Move x y 'x'

firstMove :: Char -> Move
firstMove symbol = Move 1 1 symbol

allRowMoves :: Int -> Char -> Moves
allRowMoves rowN playerSymbol = [move | x <- [rowN], y <- [0..2], v <- [playerSymbol], let move = (Move x y v)]

allColumnMoves :: Int -> Char -> Moves
allColumnMoves columnN playerSymbol = [move | x <- [0..2], y <- [columnN], v <- [playerSymbol], let move = (Move x y v)]

allDiagonalMoves :: Int -> Char -> Moves
allDiagonalMoves 0 playerSymbol = [Move 0 0 playerSymbol, Move 1 1 playerSymbol, Move 2 2 playerSymbol]
allDiagonalMoves 1 playerSymbol = [Move 0 2 playerSymbol, Move 1 1 playerSymbol, Move 2 0 playerSymbol]

allCornerMoves :: Char -> Moves
allCornerMoves playerSymbol = [move | x <- [0,2], y <- [0,2], v <- [playerSymbol], let move = (Move x y v)]

checkRow :: (Moves -> Moves -> Moves -> Maybe Move) -> Char -> Moves -> [Maybe Move]
checkRow criteria playerSymbol moves = map (checkByCriteria criteria playerSymbol moves allRowMoves) [0, 1, 2]

checkColumn :: (Moves -> Moves -> Moves -> Maybe Move) -> Char -> Moves -> [Maybe Move]
checkColumn criteria playerSymbol moves = map (checkByCriteria criteria playerSymbol moves allColumnMoves) [0, 1, 2]

checkDiagonal :: (Moves -> Moves -> Moves -> Maybe Move) -> Char -> Moves -> [Maybe Move]
checkDiagonal criteria playerSymbol moves = map (checkByCriteria criteria playerSymbol moves allDiagonalMoves) [0, 1]

checkByCriteria :: (Moves -> Moves -> Moves -> Maybe Move) -> Char -> Moves -> (Int -> Char -> Moves) -> Int -> Maybe Move
checkByCriteria criteria playerSymbol moves allPossibleMovesGen index = criteria possibleOurMoves possibleOpponentMoves moves
    where
        possibleOurMoves = allPossibleMovesGen index playerSymbol
        possibleOpponentMoves = allPossibleMovesGen index (opponent playerSymbol)

matchCriteria :: (Moves -> Moves -> Moves -> Maybe Move) -> Char -> Moves -> Maybe Move
matchCriteria criteria playerSymbol moves = msum (inARow ++ inAColumn ++ inADiagonal)
    where
      inARow = checkRow criteria playerSymbol moves
      inAColumn = checkColumn criteria playerSymbol moves
      inADiagonal = checkDiagonal criteria playerSymbol moves

oneToDefend :: Moves -> Moves -> Moves -> Maybe Move
oneToDefend possibleOurMoves possibleOpponentMoves moves =
    if length ourMoves == 0 && length opponentMoves == 2
        then Just (inverseMovePlayer (head (possibleOpponentMoves \\ opponentMoves)))
        else Nothing
    where
        ourMoves = possibleOurMoves `intersect` moves
        opponentMoves = possibleOpponentMoves `intersect` moves

needToWin :: Int -> Moves -> Moves -> Moves -> Maybe Move
needToWin left possibleOurMoves possibleOpponentMoves moves =
    if length ourMoves == (3 - left) && length opponentMoves == 0
        then Just (head (possibleOurMoves \\ ourMoves))
        else Nothing
    where
        ourMoves = possibleOurMoves `intersect` moves
        opponentMoves = possibleOpponentMoves `intersect` moves

randomMove :: Char -> Moves -> IO (Maybe Move)
randomMove playerSymbol moves = if movesLength > 0
    then do
        index <- randomRIO (0, movesLength - 1)
        return $ Just (availableMoves !! index)
    else return Nothing
    where
        possibleOurMoves = [move | x <- [0..2], y <- [0..2], v <- [playerSymbol], let move = (Move x y v), move `notElem` moves]
        availableMoves = filter (\move -> (inverseMovePlayer move) `notElem` moves) possibleOurMoves
        movesLength = length availableMoves

nextMove :: Char -> Moves -> IO (Maybe Move)
nextMove playerSymbol moves = do
    random <- randomMove playerSymbol moves
    return $ msum [matchCriteria (needToWin 1) playerSymbol moves, matchCriteria oneToDefend playerSymbol moves, matchCriteria (needToWin 2) playerSymbol moves, random]

winnerExists :: Moves -> Bool
winnerExists moves = do
  if (checkIfWin moves 'x')
    then True
  else if (checkIfWin moves 'o')
    then True
  else False

checkIfWin :: Moves -> Char -> Bool
checkIfWin moves ch
  | check setOfMoves (toFlatSet [Move 0 0 ch, Move 0 1 ch, Move 0 2 ch]) = True
  | check setOfMoves (toFlatSet [Move 1 0 ch, Move 1 1 ch, Move 1 2 ch]) = True
  | check setOfMoves (toFlatSet [Move 2 0 ch, Move 2 1 ch, Move 2 2 ch]) = True
  | check setOfMoves (toFlatSet [Move 0 0 ch, Move 1 0 ch, Move 2 0 ch]) = True
  | check setOfMoves (toFlatSet [Move 0 1 ch, Move 1 1 ch, Move 2 1 ch]) = True
  | check setOfMoves (toFlatSet [Move 0 2 ch, Move 1 2 ch, Move 2 2 ch]) = True
  | check setOfMoves (toFlatSet [Move 0 0 ch, Move 1 1 ch, Move 2 2 ch]) = True
  | check setOfMoves (toFlatSet [Move 2 0 ch, Move 1 1 ch, Move 0 2 ch]) = True
  | otherwise                                    = False
  where
    setOfMoves = toFlatSet moves

check :: Set.Set (Int, Int, Char) -> Set.Set (Int, Int, Char) -> Bool
check setOfMoves setOfWins = Set.isSubsetOf setOfWins setOfMoves

toFlatSet :: Moves -> Set.Set (Int, Int, Char)
toFlatSet moves = Set.fromList(makeFlatSet moves [])

makeFlatSet :: Moves -> [(Int, Int, Char)] -> [(Int, Int, Char)]
makeFlatSet (move:tail) acc =
  let
    xx = x move
    yy = y move
    vv = v move
  in makeFlatSet tail ((xx,yy,vv):acc)
makeFlatSet [] acc = acc
