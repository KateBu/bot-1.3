import Tests as Tests 
import Test.HUnit ( runTestTT )

main :: IO ()
main = do 
    putStrLn "Running tests..."
    runTestTT Tests.testRepeatMsg0
    pure ()
