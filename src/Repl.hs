{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Repl where 

import Control.Monad.Trans (liftIO)
import GHC.Generics (Generic)
import Data.Binary
import Data.Typeable
import Control.Monad (forever)
import Data.Maybe
import Text.Read (readMaybe)
import Control.Distributed.Process

import Types

data Command = Add Flower | Show
  deriving (Typeable, Generic, Read)
instance Binary Command

data Answer = Added Flower | HereUR FlowerList
  deriving (Typeable, Generic, Show)
instance Binary Answer

runRepl :: ProcessId -> Process ()
runRepl parent = do
  liftIO $ putStrLn "Welcome, scout bee. Write 'Add (x,y)' to add a flower, 'Show' to list available flowers."
  forever $ do
    line <- liftIO $ getLine
    case ((readMaybe line) :: Maybe Command) of
      Just command -> do
        send parent command
        answer <- expect :: Process Answer
        liftIO $ print answer
      Nothing -> 
        liftIO $ putStrLn "No such command"
