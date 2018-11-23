{-# LANGUAGE NamedFieldPuns #-}
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
import qualified System.Random.Shuffle as Rand
import GHC.Base ((<|>))
import Data.Aeson
import GHC.Generics
import Lib

import Debug.Trace

type ClientId = Text.Text

data ClientState = ClientState
  { generation :: Int
  , grid :: Grid
  } deriving (Show, Eq)

type State = Map.Map ClientId ClientState

data Message
  = Next
  | Start { width :: Int
          , height :: Int}
  deriving (Show, Eq)

instance FromJSON Message where
  parseJSON =
    withObject "next or start" $
    \o ->
       (do startO <- o .: "start"
           width <- startO .: "width"
           height <- startO .: "height"
           return $ Start width height) <|>
       (do next :: Int <- o .: "next"
           return Next)

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
  \state -> return $ Map.insert clientId clientState state
  where
    clientState = ClientState 0 (Grid 0 0 Map.empty)

disconnectClient :: ClientId -> Concurrent.MVar State -> IO ()
disconnectClient clientId stateRef =
  Concurrent.modifyMVar_ stateRef $ \state -> return $ Map.delete clientId state

listen :: WS.Connection -> ClientId -> Concurrent.MVar State -> IO ()
listen conn clientId stateRef =
  Monad.forever $ WS.receiveData conn >>= handleMsg conn clientId stateRef

sendGrid :: WS.Connection -> ClientId -> Grid -> IO ()
sendGrid conn clientId grid = WS.sendTextData conn $ gridToJson grid

randGrid :: Int -> Int -> IO Grid
randGrid w h =
  let numberOfRandomCells = round 0.8 * fromIntegral w * h
  in do g <- Rand.newStdGen
        let numbers :: [Int] = take (2 * numberOfRandomCells) $ Rand.randoms g
        let pairs = zip numbers $ drop numberOfRandomCells numbers
        let list = map (\(a, b) -> ((a `rem` w, b `rem` h), 1)) pairs
        return $ Grid w h $ Map.fromList list

tricksterCells :: Grid -> IO (Map.Map (Int,Int) Int)
tricksterCells grid = do
  g <- Rand.newStdGen
  let ns = allEmptyNeighbours grid
      ln = length ns
      sh = Rand.shuffle' ns ln g
  return $ Map.fromList $ zip (take (ln `div` 2) sh) (repeat 2)

handleMsg :: WS.Connection
          -> ClientId
          -> Concurrent.MVar State
          -> BS.ByteString
          -> IO ()
handleMsg conn clientId stateRef msg =
  case decode msg :: Maybe Message of
    Just Next -> do
      state <-
        Concurrent.modifyMVar stateRef $
        \s ->
           case Map.lookup clientId s of
             Just ClientState {generation
                              ,grid}
               | 0 == generation `rem` 10 &&
                   Map.size (Lib.cells grid) <
                   (Lib.width grid * Lib.height grid `div` 10) -> do
                 newCells <- tricksterCells grid
                 let newGrid = grid {Lib.cells = Map.union newCells (Lib.cells grid)}
                     newState = Map.insert clientId (ClientState (generation + 1) newGrid) s
                 sendGrid conn clientId newGrid
                 return (newState, newState)
             Just ClientState {generation
                              ,grid} -> do
               let newState =
                     Map.insert
                       clientId
                       (ClientState (generation + 1) (nextGrid grid))
                       s
               sendGrid conn clientId grid
               return (newState, newState)
             Nothing -> do
               WS.sendTextData
                 conn
                 ("{\"tag\":\"error\", \"code\":1}" :: Text.Text)
               return (s, s)
      return ()
    Just (Start w h) -> do
      grid <- randGrid w h
      state <-
        Concurrent.modifyMVar stateRef $
        \s ->
           let m = Map.insert clientId (ClientState 0 grid) s
           in return (m, m)
      return ()
    Nothing ->
      WS.sendTextData conn ("{\"tag\":\"error\", \"code\":2}" :: Text.Text)

wsApp :: Concurrent.MVar State -> WS.ServerApp
wsApp stateRef pendingConn = do
  conn <- WS.acceptRequest pendingConn
  clientId <- UUID.toText <$> UUID.nextRandom
  connectClient clientId stateRef
  WS.sendTextData
    conn
    ("{\"tag\":\"colors\", \"colors\":[ {\"color\":\"#4682B4\", \"code\":1} , {\"color\":\"#66a2d4\", \"code\":2} ]}" :: Text.Text)
  WS.forkPingThread conn 30
  Exception.finally
    (listen conn clientId stateRef)
    (disconnectClient clientId stateRef)

httpApp :: Wai.Application
httpApp =
  Static.staticApp $
  s
  { Static.ssIndices = [Static.unsafeToPiece "index.html"]
  }
  where
    s = Static.defaultWebAppSettings "./static"

main :: IO ()
main = do
  state <- Concurrent.newMVar Map.empty
  Warp.run 8080 $ WS.websocketsOr WS.defaultConnectionOptions (wsApp state) httpApp
