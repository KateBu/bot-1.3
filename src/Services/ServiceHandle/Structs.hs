module Services.ServiceHandle.Structs where

import Environment.Internals as Env (Environment)
import qualified Services.API.Handle as APIHandle
import qualified Services.DB.Handle as DBHandle

data SHandle m = SHandle
  { hAPI :: m (APIHandle.Handle m),
    hDB :: m (DBHandle.Handle m)
  }

new :: Env.Environment IO -> SHandle IO
new env =
  SHandle
    { hAPI = APIHandle.new env,
      hDB = DBHandle.new env
    }
