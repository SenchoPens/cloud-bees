{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import System.Environment (getArgs)
import Control.Distributed.Process
import Control.Distributed.Process.Node (initRemoteTable)
import GHC.Generics (Generic)
import Data.Typeable
import Data.Binary
import Data.Set (toList, union)
import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import qualified CRDT.Cv.GSet as S
import qualified Control.Distributed.Backend.P2P as P2P

import Types
import Repl

data NodeConfig = NodeConfig { -- информация, не меняющаяся во время существования ноды
  selfPid :: ProcessId,
  replPid :: ProcessId
}

data Tick = Tick                -- тип данных для того, чтобы оповещать основной поток тогда,
  deriving (Typeable, Generic)  -- когда надо переслать нодам свое состояние
instance Binary Tick  -- Tick теперь поддерживает сериализацию

runNode :: NodeConfig -> Flowers -> Process ()
runNode config@(NodeConfig _ repl) flowers = do
  let run = runNode config
  receiveWait

    [ match (\command ->
        case command of
          (Add flower) -> do
            send repl (Added flower)
            run $ S.add flower flowers
          Show -> do
            send repl (HereUR $ toList flowers)
            run flowers)

    , match (\Tick -> do
        P2P.nsendPeers "bees" flowers
        say $ show flowers
        run flowers)

    , match (\newFlowers -> do
        run $ newFlowers `union` flowers)
    ]
  
spawnNode :: Process ()
spawnNode = do
  liftIO $ threadDelay 3000000 -- give other nodes time to register
  let flowers = S.initial :: Flowers  -- инициализирум GSet координат цветков
  self <- getSelfPid
  repl <- spawnLocal $ runRepl self  -- создаем REPL в отдельном потоке
  register "bees" self  -- теперь нода будет получать сообщения из канала "bees"
  spawnLocal $ forever $ do
    send self Tick  -- оповестить основной поток что надо передать пирам свое состояние
    liftIO $ threadDelay $ 10^6  -- ждемс 0.1 секунды перед тем, как снова отослать состояние
  runNode (NodeConfig {selfPid=self, replPid=repl}) flowers

main = do  -- создаем монаду типа IO (), где будем совершать все действия
  [port, bootstrapPort] <- getArgs  -- считываем порт ноды и bootstrap ноды
  P2P.bootstrap  -- функция инициализации ноды 
    "127.0.0.1" port  -- IP и порт ноды
    [P2P.makeNodeId ("127.0.0.1:" ++ bootstrapPort)]  -- список bootstrap нод 
    initRemoteTable  -- создаем remote table
    spawnNode  -- передаем функцию запуска ноды, ее код мы напишем потом
