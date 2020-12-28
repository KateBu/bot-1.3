module API.WrapStructs where

import qualified Data.Text as T 

data HostPath = HostPath T.Text [T.Text]  
    deriving Show 

data Method = Update | Send deriving Show 