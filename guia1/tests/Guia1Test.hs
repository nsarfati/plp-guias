module Main where

import Guia1
import Test.HUnit
import qualified System.Exit as Exit
 
tests_ej2 :: Test
tests_ej2 = TestList [
    TestLabel "test_curry" (TestCase (assertEqual "curry' max2 3 7: 7" 7 (curry' max2 3 7))),
    TestLabel "test_uncurry" (TestCase (assertEqual "uncurry' (-) 3 2: 1" 1 (uncurry' (-) (3, 2))))
    
    ]

tests_ej3_1 :: Test
tests_ej3_1 = TestList [
    TestLabel "test_sum[]" (TestCase (assertEqual "sum' [] : 0" 0 (sum' []))),
    TestLabel "test_sum[1]" (TestCase (assertEqual "sum' [1] : 1" 1 (sum' [1]))),
    TestLabel "test_sum[0, 1, 3, 4, 5]" (TestCase (assertEqual "sum' [0, 1, 3, 4, 5] : 1" 13 (sum' [0, 1, 3, 4, 5]))),

    TestLabel "test_elem'(2, [])" (TestCase (assertEqual "elem'(2, []) : False" False (elem' 2 []))),
    TestLabel "test_elem'(2, [3,4,1])" (TestCase (assertEqual "elem'(2, [3,4,1]) : False" False (elem' 2 [3,4,1]))),
    TestLabel "test_elem'(2, [3,4,2,1])" (TestCase (assertEqual "elem'(2, [3,4,2,1]) : True" True (elem' 2 [3,4,2,1]))),
    TestLabel "test_elem'(2, [3,2,4,2,2,1])" (TestCase (assertEqual "elem'(2, [3,2,4,2,2,1]) : True" True (elem' 2 [3,2,4,2,2,1]))),

    TestLabel "test_[]+++[]" (TestCase (assertEqual "[]+++[] : []" [] ([] +++ []))),
    TestLabel "test_[1]+++[1]" (TestCase (assertEqual "[]+++[] : []]" [1,1] ([1] +++ [1])))
    ]

tests_ej4 :: Test
tests_ej4 = TestList [

    ]

tests_ej5 :: Test
tests_ej5 = TestList [

    ]

main :: IO ()
main = do
    result2 <- runTestTT tests_ej2
    result3 <- runTestTT tests_ej3_1
    result4 <- runTestTT tests_ej4
    result5 <- runTestTT tests_ej5
    if failures result2 > 0 || failures result3 > 0 || failures result4 > 0 || failures result5 > 0 then Exit.exitFailure else Exit.exitSuccess
