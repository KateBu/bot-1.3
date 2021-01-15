module Logic.Structs.Functions where

import qualified Environment.Environment as Env
import Logic.Structs.Messages (PureMessage)

type SendFunction m =
  (Env.Env m -> PureMessage -> m (Env.Env m))
