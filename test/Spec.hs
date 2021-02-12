import Test.HUnit (runTestTT)
import qualified Tests

main :: IO ()
main = do
  putStrLn "Config.configSetOffset function tests:"
  mapM_ runTestTT Tests.testConfigSetOffset
  putStrLn "Logic.ProcMsgs.Common.processCommonMsgs function tests:"
  mapM_ runTestTT Tests.testProcessCommonMsgs
  putStrLn "Logic.ProcMsgs.Callback.processCallbackMsgs function tests:"
  mapM_ runTestTT Tests.testProcessCallbackMsgs
  putStrLn "Logic.processMsgs function tests:"
  mapM_ runTestTT Tests.testProcessMsgs
