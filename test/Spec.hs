import Test.HUnit (runTestTT)
import Tests as Tests


main :: IO ()
main = do
    putStrLn "Config.configSetOffset function tests:"
    mapM_ runTestTT Tests.testConfigSetOffset
    putStrLn "Config.configGetUid function tests:"
    mapM_ runTestTT Tests.testConfigGetUid 
    putStrLn "Logic.ProcMsgs.Callback.makeCallbackResponse function tests:"
    runTestTT Tests.testMakeCallbackResponse
    putStrLn "Logic.makeRepeatMsg function tests:"
    mapM_ runTestTT Tests.testMakeRepeatMsg
    putStrLn "Logic.ProcMsgs.Common.repeatMsg function tests:"
    mapM_ runTestTT Tests.testsRepeatMsg
    putStrLn "Logic.ProcMsgs.Common.processMsgsCommon function tests:"
    mapM_ runTestTT Tests.testProcessMsgsCommon
  {-
  
  
  putStrLn "Logic.processMsgs function tests:"
  mapM_ runTestTT Tests.testProcessMsgs
  putStrLn "Logic.processMsgs_ function tests:"
  mapM_ runTestTT Tests.testProcessMsgs_
  putStrLn "Logic.processMsgsCallback function tests:"
  mapM_ runTestTT Tests.testProcessMsgsCallback

  
  
  
-}