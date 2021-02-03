module TestData.TestServices where

import qualified Data.Map as Map
import qualified Environment.Exports as Env
import qualified Logic.PureStructs as PureStructs
import qualified Services.API.Handle as API
import qualified Services.DB.Handle as DB
import qualified Services.SHandle as Services
import TestData.TestEnvironment (testEnvTelegram, testEnvVK)
import qualified TestData.TestMessages as Msgs

servicesVk1 :: Services.SHandle Maybe
servicesVk1 =
  Services.SHandle
    { Services.hAPI = pure apiVK1,
      Services.hDB = pure db
    }

servicesTel1 :: Services.SHandle Maybe
servicesTel1 =
  Services.SHandle
    { Services.hAPI = pure apiTel1,
      Services.hDB = pure db
    }

testDataBase :: Map.Map Int Int
testDataBase = Map.fromList [(11, 1), (22, 2), (33, 3)]

apiVK1 :: API.Handle Maybe
apiVK1 =
  API.Handle
    { API.hGetUpdates = pure Msgs.allMessages,
      API.hSendMessage = sendMsg1 testEnvVK
    }

apiTel1 :: API.Handle Maybe
apiTel1 =
  API.Handle
    { API.hGetUpdates = pure Msgs.allMessages,
      API.hSendMessage = sendMsg1 testEnvTelegram
    }

db :: DB.Handle Maybe
db =
  DB.Handle
    { DB.findUser = \uid -> pure $ Map.lookup uid testDataBase,
      DB.addUser = \_ _ -> pure (),
      DB.updateUser = \_ _ -> pure ()
    }

sendMsg1 :: Env.Environment Maybe -> PureStructs.PureMessage -> Maybe (Env.Environment Maybe)
sendMsg1 env msg = Env.eSetOffset env $ succ (PureStructs.updateID msg)
