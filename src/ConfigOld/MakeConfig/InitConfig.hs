module ConfigOld.MakeConfig.InitConfig where

import qualified ConfigOld.ConfigStructs as Config
import Control.Monad ()
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Exceptions.Exceptions as BotEx

initConfig ::
  Maybe T.Text ->
  Maybe Int ->
  Maybe String ->
  Config.BotType ->
  IO Config.Config
initConfig (Just helpMsg) (Just rep) (Just prior) botType = do
  pure $ Config.Config botType helpMsg (checkRepNumber rep) Map.empty (read prior)
initConfig _ _ _ _ = BotEx.throwInitConfigExcept

checkRepNumber :: Int -> Int
checkRepNumber val
  | val <= 1 = 1
  | val >= 5 = 5
  | otherwise = val
