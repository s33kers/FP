module Parser
where

import Data.String as S
import Data.Char
import Models

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

decodeScala :: String -> Moves
decodeScala ('L':'i':'s':'t':'(':listRest) = parseTuples [] listRest
decodeScala a = error ("Parsing exe " ++ a)

parseTuples acc ")" = acc
parseTuples acc (' ':rest) = parseTuples acc rest
parseTuples acc ('M':'a':'p':mapRest) =
  let
    (tuple, tupleRest) = parseTuple mapRest
    separatorRest = readSeparator tupleRest
  in
    parseTuples (tuple:acc) separatorRest
parseTuples acc _ = error "Bad map declaration"

parseTuple :: String -> (Move, String)
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
      (')':t) -> ((Move x y v), t)
      _       -> error "Tuple without closing bracket"


encodeScala :: Moves -> String
encodeScala moves = "List("++ (encodeMoves moves "" ) ++ ")"

encodeMoves :: Moves -> String -> String
encodeMoves (move:tail) acc =
  let
    xx = x move
    yy = y move
    vv = v move

  in
  if tail /= []
    then encodeMoves tail (acc ++ (encodeMove xx yy vv) ++ ",")
    else encodeMoves tail (acc ++ (encodeMove xx yy vv))
encodeMoves [] acc = acc

encodeMove :: Int -> Int -> Char -> String
encodeMove x y v = ("Map(x -> " ++ xString ++ ", y -> " ++ yString ++ ", v -> " ++ vString ++ ")")
  where
    xString = show x
    yString = show y
    vString = [v]
