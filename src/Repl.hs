{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Repl where

import Control.Distributed.Process
import Control.Monad (forever)
import Control.Monad.Trans (liftIO)
import Data.Binary
import Data.Maybe
import Data.Typeable
import GHC.Generics (Generic)
import Text.Read (readMaybe)

import Types

-- тип комманды
data Command
  = Add Flower
  | Show
  deriving (Typeable, Generic, Read) -- с помощью Read можно считывать комманду из REPL

instance Binary Command

-- тип ответа от ноды
data Answer
  = Added Flower
  | HereUR FlowerList
  deriving (Typeable, Generic, Show)

instance Binary Answer

runRepl :: ProcessId -> Process ()
runRepl parent = do
  liftIO $
    putStrLn
      "Welcome, scout bee. Write 'Add (x,y)' to add a flower, 'Show' to list available flowers."
  forever $ do
    line <- liftIO getLine -- считываем команду
    case (readMaybe line :: Maybe Command) of
      Just command -- если пользователь ввел нормальную команду
       -> do
        send parent command -- отправляем ее главному процессу
        answer <- expect :: Process Answer -- ожидаем результат
        liftIO $ print answer
      Nothing -- пользователь ввел фигню и read не смог это прочитать
       -> liftIO $ putStrLn "No such command"
