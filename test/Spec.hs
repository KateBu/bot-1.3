import Tests as Tests 
import Test.HUnit ( runTestTT )

main :: IO ()
main = do 
    putStrLn "Config.setUserRepeat function tests:"
    mapM_ runTestTT Tests.testSetUserRepeat
    putStrLn "Config.reWriteUserRepeat function tests:"
    mapM_ runTestTT Tests.testReWriteUserRepeat
    putStrLn "Config.findUserRepeat function tests:"
    mapM_ runTestTT Tests.testFindUserRepeat
    putStrLn "Config.deleteUser function tests:"
    mapM_ runTestTT Tests.testDeleteUser
    putStrLn "Config.addUser function tests:"
    mapM_ runTestTT Tests.testAddUser
    putStrLn "Config.configSetOffset function tests:"
    mapM_ runTestTT Tests.testConfigSetOffset
    putStrLn "Logic.getLastConf function tests:"
    mapM_ runTestTT Tests.testGetLastConf
    putStrLn "Logic.processMsgs function tests:"
    mapM_ runTestTT Tests.testProcessMsgs
    putStrLn "Logic.processMsgs_ function tests:"
    mapM_ runTestTT Tests.testProcessMsgs_
    putStrLn "Logic.processMsgsCallback function tests:"
    mapM_ runTestTT Tests.testProcessMsgsCallback
    putStrLn "Logic.processMsgsCommon function tests:"
    mapM_ runTestTT Tests.testProcessMsgsCommon
    putStrLn "Logic.makeRepeatMsg function tests:"
    mapM_ runTestTT Tests.testMakeRepeatMsg
    putStrLn "Logic.makeCallbackResponse function tests:"
    runTestTT Tests.testMakeCallbackResponse 
    putStrLn "Logic.repeatMsg function tests:"
    mapM_ runTestTT Tests.testsRepeatMsg
    
