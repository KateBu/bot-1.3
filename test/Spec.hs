import Tests as Tests 
import Test.HUnit ( runTestTT )

main :: IO ()
main = do 
    putStrLn " ...Running tests..."
    mapM_ runTestTT Tests.testsRepeatMsg
