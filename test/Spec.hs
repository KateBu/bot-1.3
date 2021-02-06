import Test.HUnit (runTestTT)
import qualified Tests

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
  mapM_ runTestTT Tests.testProcessCommonMsgs
  putStrLn "Logic.ProcMsgs.Callback.processMsgsCallback function tests:"
  mapM_ runTestTT Tests.testProcessCallbackMsgs
  putStrLn "Logic.processMsgs_ function tests:"
  mapM_ runTestTT Tests.testProcessMsg
  putStrLn "Logic.processMsgs function tests:"
  mapM_ runTestTT Tests.testProcessMsgs
