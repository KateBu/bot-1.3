module Logic.PureStructs (module PureStructs) where

import Logic.Messages as PureStructs (newRepeatText, repeatText)
import Logic.Structs.Buttons as PureStructs
  ( PureButtons (..),
    buttons',
    getNewRep,
    rep1,
    rep2,
    rep3,
    rep4,
    rep5,
  )
import Logic.Structs.Messages as PureStructs
  ( ChatID,
    MbCaption,
    MessageType (..),
    Params (..),
    PureMessage (..),
    UCommand (..),
    UpdateID,
  )
