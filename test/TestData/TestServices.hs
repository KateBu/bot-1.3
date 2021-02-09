module TestData.TestServices where

import qualified Data.Map as Map
import qualified Environment.Exports as Env
import qualified Logic.Structs as PureStructs
import qualified Services.Main as Services
import qualified TestData.TestMessages as Msgs

testDataBase :: Map.Map Int Int
testDataBase = Map.fromList [(11, 1), (22, 2), (33, 3)]

instance Services.Services Maybe where
  getUpdates = \_ -> pure Msgs.allMessages
  sendMessage = \env msg -> Env.eSetOffset env $ succ (PureStructs.updateID msg)
  findUser = findUserMaybe
  addUser = \_ _ _ -> Just ()
  updateUser = \_ _ _ -> Just ()

findUserMaybe :: Env.Environment Maybe -> PureStructs.ChatID -> Maybe (Maybe Env.RepeatNumber)
findUserMaybe _ chatId = pure $ Map.lookup chatId testDataBase
