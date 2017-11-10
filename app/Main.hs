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

data NodeConfig = NodeConfig ProcessId -- информация, не меняющаяся во время существования ноды

data Tick = Tick                -- тип данных для того, чтобы оповещать основной поток тогда,
  deriving (Typeable, Generic)  -- когда надо переслать нодам свое состояние
instance Binary Tick  -- Tick теперь поддерживает сериализацию

runNode :: NodeConfig -> Flowers -> Process ()  -- функция выполнения ноды
runNode config@(NodeConfig repl) flowers = do
  let run = runNode config
  receiveWait  -- ждем сообщений
    [ match (\command ->  -- если нам пришло что-то типа Command от REPL:
        case command of
          (Add flower) -> do  -- команда добавления элемента от пользователя
            send repl (Added flower)    -- отправить REPLу сообщение, что цветок добавлен
            run $ S.add flower flowers  -- запускаем ее уже с новым цветком
          Show -> do  -- запрос показать цветочки
            send repl (HereUR $ toList flowers)  -- отправить цветочки в виде списка
            run flowers)

    , match (\Tick -> do  -- сигнал о том, что надо поделиться своим состоянием с другими
        P2P.nsendPeers "bees" flowers  -- отправить всем пирам цветки
        run flowers)

    , match (\newFlowers -> do  -- кто-то отправил ноде цветочки
        run $ newFlowers `union` flowers)  -- добавляем новые в базу - по сути обьединение множеств
    ]
  
spawnNode :: Process ()
spawnNode = do
  liftIO $ threadDelay 3000000  -- дать bootstrap ноде время для запуска
  let flowers = S.initial :: Flowers  -- инициализирум GSet координат цветков
  self <- getSelfPid                 -- получаем наш Pid чтобы REPL мог посылать нам сообщения
  repl <- spawnLocal $ runRepl self  -- создаем REPL в отдельном потоке
  register "bees" self  -- теперь нода будет получать сообщения из канала "bees"
  spawnLocal $ forever $ do  -- запускаем тикер:
    send self Tick               -- оповестить основной поток что надо передать пирам свое состояние
    liftIO $ threadDelay $ 10^6  -- ждемс 0.1 секунды перед тем, как снова отослать состояние
  runNode (NodeConfig repl) flowers  -- запускаем ноду

main = do
  [port, bootstrapPort] <- getArgs  -- считываем порт ноды и bootstrap ноды
  P2P.bootstrap                     -- функция инициализации ноды 
    "127.0.0.1" port  -- IP и порт ноды
    [P2P.makeNodeId ("127.0.0.1:" ++ bootstrapPort)]  -- список bootstrap нод 
    initRemoteTable  -- создаем remote table
    spawnNode        -- передаем функцию запуска ноды, ее код мы напишем потом
