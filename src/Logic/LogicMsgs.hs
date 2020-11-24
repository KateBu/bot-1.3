module Logic.LogicMsgs where


import qualified Data.Map as Map 
import qualified Data.Text as T 

logicMsgs :: Map.Map String T.Text
logicMsgs = Map.fromList [
    ("rptCmdTxt", "Выберите количество повторов:")
    , ("newRptTxt", "Установлено количество обновлений: ")
    ]
