module TestData.TestServices where

import qualified Data.Map as Map
import qualified Environment.Environment as Env
import qualified Logic.PureStructs as PureStructs
import qualified Services.APIHandle.APIHandle as API
import qualified Services.DBHandle.DBHandle as DB
import qualified Services.SHandle as Services
import TestData.TestEnvironment (testEnvTelegram, testEnvVK)
import qualified TestData.TestMessages as Msgs

servicesVk :: Services.SHandle Maybe
servicesVk =
  Services.SHandle
    { Services.hAPI = pure apiVK,
      Services.hDB = pure db
    }

servicesTel :: Services.SHandle Maybe
servicesTel =
  Services.SHandle
    { Services.hAPI = pure apiTel,
      Services.hDB = pure db
    }

testDataBase :: Map.Map Int Int
testDataBase = Map.fromList [(11, 1), (22, 2), (33, 3)]

apiVK :: API.Handle Maybe
apiVK =
  API.Handle
    { API.hGetUpdates = pure Msgs.allMessages,
      API.hSendMessage = sendMsg testEnvVK
    }

apiTel :: API.Handle Maybe
apiTel =
  API.Handle
    { API.hGetUpdates = pure Msgs.allMessages,
      API.hSendMessage = sendMsg testEnvTelegram
    }

db :: DB.Handle Maybe
db =
  DB.Handle
    { DB.findUser = \uid -> pure $ Map.lookup uid testDataBase,
      DB.addUser = \_ _ -> pure (),
      DB.updateUser = \_ _ -> pure ()
    }

sendMsg :: Env.Environment Maybe -> PureStructs.PureMessage -> Maybe (Env.Environment Maybe)
sendMsg env msg = do
  Env.eSetOffset env $ succ (PureStructs.updateID msg)
