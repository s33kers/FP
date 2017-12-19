module Main where

import System.Environment
import Game
import Models

main :: IO ()
main =  do
  -- play $ GameSetup Attack "kazkasnutiko1" "1" 'x'
  args <- getArgs
  case args of
    ["attack", gameId, "x"] -> play $ Setup Attack gameId "1" 'x'
    ["attack", gameId, "o"] -> play $ Setup Attack gameId "1" 'o'
    ["defend", gameId, "x"] -> play $ Setup Defend gameId "2" 'x'
    ["defend", gameId, "o"] -> play $ Setup Defend gameId "2" 'o'
    _ -> putStrLn "No combination found"
