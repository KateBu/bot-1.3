module TestData.TestFunctions where

import qualified Config.Config as Config
import qualified Data.Text as T
import qualified Logic.Logic as Logic
import qualified Logic.PureStructs as PureStructs
{-
testFunction0 :: Logic.SendFunction T.Text Maybe
testFunction0 _ _ (PureStructs.PureMessage PureStructs.MTEmpty _ _ _) =
  Nothing
testFunction0 config newHelpMessage _ = pure $ Right $ config {Config.helpMessage = newHelpMessage}

testFunction1 :: Logic.SendFunction Int Maybe
testFunction1 _ _ (PureStructs.PureMessage PureStructs.MTEmpty _ _ _) = Nothing
testFunction1 config num _ =
  if num < 0
    then Nothing
    else
      let uid = Config.configGetUid config
       in pure $ Right (Config.configSetOffset config (succ uid))
-}