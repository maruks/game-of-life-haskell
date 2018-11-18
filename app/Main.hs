{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as BS
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets as WS
import qualified Network.Wai.Application.Static as Static
import qualified WaiAppStatic.Types as Static
import qualified Data.UUID.V4 as UUID
import qualified Data.UUID as UUID
import qualified System.Random as Rand
import Data.Aeson
import GHC.Generics
import Lib

type ClientId = Text.Text

type State = Map.Map ClientId Grid

data GridSize = GridSize
  { width :: Int
  , height :: Int
  } deriving (Show, Generic)

instance FromJSON GridSize

data Start = Start
  { start :: GridSize
  } deriving (Show, Generic)

instance FromJSON Start

data Next = Next
  { next :: Int
  } deriving (Show, Generic)

instance FromJSON Next

data Point = Point
  { color :: Int
  , point :: (Int, Int)
  } deriving (Show, Generic)

instance ToJSON Point

data Cells = Cells
  { tag :: String
  , cells :: [Point]
  } deriving (Show, Generic)

instance ToJSON Cells

gridToJson :: Grid -> BS.ByteString
gridToJson (Grid _ _ cells) = encode $ Cells "cells" points
  where
    points = List.map (\(point, color) -> Point color point) $ Map.assocs cells

connectClient :: ClientId -> Concurrent.MVar State -> IO ()
connectClient clientId stateRef =
  Concurrent.modifyMVar_ stateRef $
  \state -> return $ Map.insert clientId (Grid 0 0 Map.empty) state

disconnectClient :: ClientId -> Concurrent.MVar State -> IO ()
disconnectClient clientId stateRef =
  Concurrent.modifyMVar_ stateRef $ \state -> return $ Map.delete clientId state

listen :: WS.Connection -> ClientId -> Concurrent.MVar State -> IO ()
listen conn clientId stateRef =
  Monad.forever $ WS.receiveData conn >>= handleMsg conn clientId stateRef

sendGrid :: WS.Connection -> ClientId -> State -> IO ()
sendGrid conn clientId state =
  case Map.lookup clientId state of
    Just g -> WS.sendTextData conn $ gridToJson g
    Nothing ->
      WS.sendTextData conn ("{\"tag\":\"error\", \"code\":1}" :: Text.Text)

randGrid :: Int -> Int -> IO Grid
randGrid w h =
  let alive = w * h `div` 2
  in do g <- Rand.getStdGen
        let numbers :: [Int] = take (2 * alive) $ Rand.randoms g
        let pairs = zip numbers $ drop alive numbers
        let list = map (\(a, b) -> ((mod a w, mod b h), 1)) pairs
        return $ Grid w h $ Map.fromList list

handleMsg :: WS.Connection
          -> ClientId
          -> Concurrent.MVar State
          -> BS.ByteString
          -> IO ()
handleMsg conn clientId stateRef msg =
  case decode msg :: Maybe Next of
    Just (Next x) -> do
      state <-
        Concurrent.modifyMVar stateRef $
        \s ->
           let m = Map.adjust nextGrid clientId s
           in return (m, m)
      sendGrid conn clientId state
    Nothing ->
      case decode msg :: Maybe Start of
        Just (Start (GridSize w h)) -> do
          grid <- randGrid w h
          state <-
            Concurrent.modifyMVar stateRef $
            \s ->
               let m = Map.insert clientId grid s
               in return (m, m)
          sendGrid conn clientId state
        Nothing ->
          WS.sendTextData conn ("{\"tag\":\"error\", \"code\":2}" :: Text.Text)

wsApp :: Concurrent.MVar State -> WS.ServerApp
wsApp stateRef pendingConn = do
  conn <- WS.acceptRequest pendingConn
  clientId <- UUID.toText <$> UUID.nextRandom
  connectClient clientId stateRef
  WS.sendTextData
    conn
    ("{\"tag\":\"colors\", \"colors\":[ {\"color\":\"#5F9EA0\", \"code\":1} ]}" :: Text.Text)
  WS.forkPingThread conn 30
  Exception.finally
    (listen conn clientId stateRef)
    (disconnectClient clientId stateRef)

httpApp :: Wai.Application
httpApp =
  Static.staticApp $ s { Static.ssIndices = [Static.unsafeToPiece "index.html"]}
  where
    s = Static.defaultWebAppSettings "./static"

main :: IO ()
main = do
  state <- Concurrent.newMVar Map.empty
  Warp.run 8080 $ WS.websocketsOr WS.defaultConnectionOptions (wsApp state) httpApp
