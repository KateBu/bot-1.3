module Logic.Structs.Functions where

import qualified Environment.Environment as Env
import Logic.Structs.Messages (PureMessage)

type SendFunction m =
  (Env.Environment m -> PureMessage -> m (Env.Environment m))
