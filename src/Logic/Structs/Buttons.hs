module Logic.Structs.Buttons where

import qualified Data.Text as T

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

rep1 :: T.Text
rep1 = "/setRepetition1"

rep2 :: T.Text
rep2 = "/setRepetition2"

rep3 :: T.Text
rep3 = "/setRepetition3"

rep4 :: T.Text
rep4 = "/setRepetition4"

rep5 :: T.Text
rep5 = "/setRepetition5"

getNewRep :: T.Text -> Int
getNewRep txt
  | txt == rep1 = 1
  | txt == rep2 = 2
  | txt == rep3 = 3
  | txt == rep4 = 4
  | otherwise = 5
