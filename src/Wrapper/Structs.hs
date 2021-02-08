module Wrapper.Structs where

import qualified Data.Text as T

data HostPath = HostPath T.Text [T.Text]
  deriving (Show)

data Method = GetUpdate | Send deriving (Show)
