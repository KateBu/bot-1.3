module Wrapper.URL.Structs where

import qualified Data.Text as T

data HostPath = HostPath T.Text [T.Text]
  deriving (Show)
