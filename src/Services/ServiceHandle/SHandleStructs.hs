module Services.ServiceHandle.SHandleStructs where

import Environment.Environment as Env (Environment)
import qualified Services.APIHandle.APIHandle as APIHandle
import qualified Services.DBHandle.DBHandle as DBHandle

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
