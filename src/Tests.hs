--Some simple tests to check validity of my implementation.

module Tests (main) where 

import RegexMatcher

testCase :: String -> String -> Bool -> IO ()
testCase regex input expected =
  let result = matchRegex regex input
  in if result == expected
        then putStrLn $ "[PASS] matchRegex " ++ show regex ++ " " ++ show input
        else putStrLn $ "[FAIL] " ++ show (regex, input) ++ " expected: " ++ show expected ++ " got: " ++ show result

main :: IO ()
main = do
  testCase "a*b" "aaab" True
  testCase "(a|b)*c" "abababac" True
  testCase "(a|b)*c" "aa" False
  testCase "ab|cd" "cd" True
  testCase "ab|cd" "abcd" False
  testCase "a" "a" True
  testCase "a" "b" False
  testCase "" "" True 
