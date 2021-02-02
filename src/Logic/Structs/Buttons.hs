module Logic.Structs.Buttons where

import qualified Data.Text as T
import qualified Environment.Internals as Env
import Logic.Structs.PureMessage (CallbackText)

data PureButtons = PureButtons T.Text T.Text
  deriving (Show)

buttons' :: [[PureButtons]]
buttons' =
  [ [PureButtons "1" rep1],
    [PureButtons "2" rep2],
    [PureButtons "3" rep3],
    [PureButtons "4" rep4],
    [PureButtons "5" rep5]
  ]

rep1 :: CallbackText
rep1 = "/setRepetition1"

rep2 :: CallbackText
rep2 = "/setRepetition2"

rep3 :: CallbackText
rep3 = "/setRepetition3"

rep4 :: CallbackText
rep4 = "/setRepetition4"

rep5 :: CallbackText
rep5 = "/setRepetition5"

getNewRep :: CallbackText -> Env.RepeatNumber
getNewRep txt
  | txt == rep1 = 1
  | txt == rep2 = 2
  | txt == rep3 = 3
  | txt == rep4 = 4
  | otherwise = 5
