module Logic.PureStructs (module PureStructs) where

import Logic.Structs.Messages as PureStructs
    ( Params(..),
      UCommand(..),
      PureMessage(..),
      MessageType(..),
      MbCaption,
      ChatID,
      UpdateID )
import Logic.Structs.Buttons as PureStructs
    ( PureButtons(..),
      buttons',
      rep1,
      rep2,
      rep3,
      rep4,
      rep5,
      getNewRep ) 
import Logic.TextData as PureStructs ( repeatText, newRepeatText ) 
