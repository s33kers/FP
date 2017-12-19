{-# LANGUAGE OverloadedStrings #-}

module Connection
where

import Models
import Parser
import Network.HTTP.Simple
import Data.ByteString.Lazy as BS
import Data.ByteString.Char8 as C8
import Language.Haskell.TH.Ppr as TH
import Control.Monad

gameUrl :: String -> String -> String
gameUrl gId pId = "http://tictactoe.homedir.eu/game/" ++ gId ++ "/player/" ++ pId

getMoves :: String -> String -> IO (Moves)
getMoves gId pId = do
    request' <- parseRequest getUrl
    let request = setRequestHeader "Accept" ["application/scala"] $ request'
    response <- httpLBS request
    print $ "GETTING"
    void $ print $ getResponseStatusCode response
    return $  (decodeScala (TH.bytesToString (BS.unpack (getResponseBody response))))
    where
      getUrl = gameUrl gId pId

sendMoves :: String -> String -> Moves -> IO ()
sendMoves gId pId moves = do
    request' <- parseRequest postUrl
    let request = setRequestBodyLBS encodedMovesBytes $ setRequestHeader "Content-Type" ["application/scala"]  $ request'
    response <- httpLBS request
    print $ "SENDING"
    print $ encodeScala moves
    void $ print $ getResponseStatusCode response
    where
      encodedMoves = encodeScala moves
      encodedMovesBytes = BS.fromStrict (C8.pack (encodedMoves))
      postUrl = "POST " ++ (gameUrl gId pId)
