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

    TestLabel "test_[]+++[]" (TestCase (assertEqual "[]+++[] : []" [1] ([1] +++ []))),
    TestLabel "test_[]+++[]" (TestCase (assertEqual "[]+++[] : []" [1] ([] +++ [1]))),
    TestLabel "test_[1]+++[1]" (TestCase (assertEqual "[]+++[] : []]" [1, 1] ([1] +++ [1]))),

    TestLabel "test_filter(> 1, [])" (TestCase (assertEqual "filter(> 1, []) : []" [] (filter' (< 1) []))),
    TestLabel "test_filter(> 1, [-4,0,1])" (TestCase (assertEqual "filter(> 1, [-4,0,1]) : []" [] (filter' (> 1) [-4, 0, 1]))),
    TestLabel "test_filter(> 1, [4,0,2,1,5])" (TestCase (assertEqual "filter(> 1, [4,0,2,1,5]) : [4,2,5]" [4,2,5] (filter' (> 1) [4, 0, 2, 1, 5]))),

    TestLabel "test_map((*2), [])" (TestCase (assertEqual "map((*2), []) : []" [] (map' (*2) []))),
    TestLabel "test_map((*2), [0,1,2])" (TestCase (assertEqual "map((*2), [0,1,2]) : [0,2,4]" [0, 2, 4] (map' (*2) [0, 1, 2]))),

    TestLabel "test_maximum'([-1,8,0,3])" (TestCase (assertEqual "maximum([-1,8,0,3]) : 8" 8 (maximum' [-1, 8, 0, 3]))),

    TestLabel "test_sumasParciales([])" (TestCase (assertEqual "sumasParciales([]) : []" [] (sumasParciales []))),
    TestLabel "test_sumasParciales([1])" (TestCase (assertEqual "sumasParciales([1]) : [1]" [1] (sumasParciales [1]))),
    TestLabel "test_sumasParciales([1,4,-1,0,5])" (TestCase (assertEqual "sumasParciales([1,4,-1,0,5]) : [1,4,-1,0,5]" [1,5,4,4,9] (sumasParciales [1,4,-1,0,5]))),

    TestLabel "test_sumasAlt([])" (TestCase (assertEqual "sumasAlt([]) : 0" 0 (sumaAlt []))),
    TestLabel "test_sumasAlt([1])" (TestCase (assertEqual "sumasAlt([1]) : 1" 1 (sumaAlt [1]))),
    TestLabel "test_sumasAlt([1,2])" (TestCase (assertEqual "sumasAlt([1,2]) : -1" (-1) (sumaAlt [1,2]))),
    TestLabel "test_sumasAlt([1,2,3])" (TestCase (assertEqual "sumasAlt([1,2,3]) : 2" 2 (sumaAlt [1,2,3]))),
    TestLabel "test_sumasAlt([1,2,3,4])" (TestCase (assertEqual "sumasAlt([1,2,3,4]) : -2" (-2) (sumaAlt [1,2,3,4]))),
    TestLabel "test_sumasAlt([1,2,3,4,5])" (TestCase (assertEqual "sumasAlt([1,2,3,4,5]) : 3" 3 (sumaAlt [1,2,3,4,5]))),
    TestLabel "test_sumaAlt[11,2,8,5])" (TestCase (assertEqual "sumaAlt[11,2,8,5] : 12" 12 (sumaAlt [11,2,8,5]))),

    TestLabel "test_sumaAltRev[11,2,8,5])" (TestCase (assertEqual "sumaAltRev[11,2,8,5] : -12" (-12) (sumaAltRev [11,2,8,5]))),
    TestLabel "test_sumaAltRev'[11,2,8,5])" (TestCase (assertEqual "sumaAltRev'[11,2,8,5] : -12" (-12) (sumaAltRev' [11,2,8,5])))

    ]

tests_ej8 :: Test
tests_ej8 = TestList [
    TestLabel "test_mapPares (x y -> (x + y)) [(1,2), (5,4), (3,2)]" (TestCase (assertEqual "mapPares (x y -> (x + y)) [(1,2), (5,4), (3,2)]: [3, 9, 5]" [3, 9 ,5] (mapPares (\x y -> (x + y)) [(1,2), (5,4), (3,2)]))),

    TestLabel "test_mapDoble (+) [1,2,3] [4,5,6]" (TestCase (assertEqual "mapDoble (+) [1,2,3] [4,5,6]" [5, 7, 9] (mapDoble (+) [1,2,3] [4,5,6])))

    ]

tests_ej9 :: Test
tests_ej9 = TestList [
    TestLabel "test_sumaMat [ [1,2,3], [4,5,6], [7,8,9] ] [ [1,1,1], [1,1,1], [1,1,1] ]" (TestCase (assertEqual "sumaMat [ [1,2,3], [4,5,6], [7,8,9] ] [ [1,1,1], [1,1,1], [1,1,1] ]: [[2,3,4],[5,6,7],[8,9,10]]" [[2,3,4],[5,6,7],[8,9,10]] (sumaMat [ [1,2,3], [4,5,6], [7,8,9] ] [ [1,1,1], [1,1,1], [1,1,1] ]))),

    TestLabel "test_trasponer [ [1, 2, 3], [4, 5, 6], [7, 8, 9] ]" (TestCase (assertEqual "trasponer [ [1, 2, 3], [4, 5, 6], [7, 8, 9] ]: [ [1, 4, 7], [2, 5, 8], [3, 6, 9] ]" [ [1, 4, 7], [2, 5, 8], [3, 6, 9] ] (trasponer [ [1, 2, 3], [4, 5, 6], [7, 8, 9] ])))

    ]

main :: IO ()
main = do
    result2 <- runTestTT tests_ej2
    result3 <- runTestTT tests_ej3_1
    result8 <- runTestTT tests_ej8
    result9 <- runTestTT tests_ej9
    if failures result2 > 0 || failures result3 > 0 || failures result8 > 0 || failures result9 > 0 then Exit.exitFailure else Exit.exitSuccess
