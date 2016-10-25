module FP_v1
where

import Data.Char
import qualified Data.Set   as Set

message :: String
message = "List(Map(x ->  0, y  ->  2,   v ->   x), Map(x   ->   1, y  ->   1,   v  ->  o),   Map(x ->  0,   y   ->  0,   v  ->  x),  Map(x ->   0, y -> 1,  v ->  o),   Map(x   ->  1,   y -> 2,   v   ->  x), Map(x ->  1,   y ->   0,  v ->   o), Map(x  -> 2, y -> 0,  v  -> x),  Map(x ->  2,   y -> 1,   v   ->   o))"
play :: String
play =
  let
    movesList = parse message
    xTuplesList = findMoves 'x' movesList
    oTuplesList = findMoves 'o' movesList
  in
    if checkIfWin xTuplesList then "X wins the game"
    else if checkIfWin oTuplesList then "O wins the game"
    else "No one wins"

checkIfWin :: [(Int, Int)] -> Bool
checkIfWin moves
  | check setOfMoves (toSet [(0,0),(0,1),(0,2)]) = True
  | check setOfMoves (toSet [(1,0),(1,1),(1,2)]) = True
  | check setOfMoves (toSet [(2,0),(2,1),(2,2)]) = True
  | check setOfMoves (toSet [(0,0),(1,0),(2,0)]) = True
  | check setOfMoves (toSet [(0,1),(1,1),(2,1)]) = True
  | check setOfMoves (toSet [(0,2),(1,2),(2,2)]) = True
  | check setOfMoves (toSet [(0,0),(1,1),(2,2)]) = True
  | check setOfMoves (toSet [(2,0),(1,1),(0,2)]) = True
  | otherwise                                    = False
  where setOfMoves = toSet moves

check :: Set.Set (Int, Int) -> Set.Set (Int, Int) -> Bool
check setOfMoves setOfWins = Set.isSubsetOf setOfWins setOfMoves

findMoves :: Char -> [(Int, Int, Char)] -> [(Int, Int)]
findMoves player movesList = [(a, b) | (a, b, c) <- movesList, c == player]

toSet :: [(Int, Int)] -> Set.Set (Int, Int)
toSet x = Set.fromList x

firstCharSeparator :: String -> Bool
firstCharSeparator (head:tail) =
  if head == ','
  then True
  else False

readSeparator :: String -> String
readSeparator (' ':rest) = readSeparator rest
readSeparator (',':rest) = rest
readSeparator (')':rest) = (')':rest)
readSeparator _ = error "Separator expected"

readMapKey :: String -> String
readMapKey (' ':rest) = readMapKey rest
readMapKey ('x':rest) = readArrow rest
readMapKey ('y':rest) = readArrow rest
readMapKey ('v':rest) = readArrow rest
readMapKey _ = error "Map key expected"

readArrow :: String -> String
readArrow (' ':rest) = readArrow rest
readArrow ('-':'>':rest) = rest
readArrow _ = error "Arrow expected"

readDigit :: String -> (Int, String)
readDigit (' ':rest) = readDigit rest
readDigit ('0':rest) = (0, rest)
readDigit ('1':rest) = (1, rest)
readDigit ('2':rest) = (2, rest)
readDigit _ = error "Digit expected"

readMove :: String -> (Char, String)
readMove (' ':rest) = readMove rest
readMove ('x': rest) = ('x', rest)
readMove ('o': rest) = ('o', rest)
readMove _ = error "Move expected"

parse :: String -> [(Int, Int, Char)]
parse ('L':'i':'s':'t':'(':listRest) = parseTuples [] listRest
parse _ = error "Bad list declaration"

parseTuples acc ")" = acc
parseTuples acc (' ':rest) = parseTuples acc rest
parseTuples acc ('M':'a':'p':mapRest) =
  let
    (tuple, tupleRest) = parseTuple mapRest
    separatorRest = readSeparator tupleRest
  in
    parseTuples (tuple:acc) separatorRest
parseTuples acc _ = error "Bad map declaration"

parseTuple :: String -> ((Int, Int, Char), String)
parseTuple ('(':rest) =
  let
    xKey = readMapKey rest
    (x, restx) = readDigit xKey
    sep1Rest = readSeparator restx
    yKey = readMapKey sep1Rest
    (y, resty) = readDigit yKey
    sep2Rest = readSeparator resty
    vKey = readMapKey sep2Rest
    (v, restp) = readMove vKey
  in
    case restp of
      (')':t) -> ((x, y, v), t)
      _       -> error "Tuple without closing bracket"
