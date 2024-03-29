module Main where

import Guia1
import Test.HUnit
import qualified System.Exit as Exit
 
tests_ej2 :: Test
tests_ej2 = TestList [
    TestLabel "test_absoluto_1" (TestCase (assertEqual "Valor Absoluto de -1.4 es: 1.4" 1.4 (valorAbsoluto (-1.4)))),
    TestLabel "test_absoluto_2" (TestCase (assertEqual "Valor Absoluto de 4.12 es: 4.12" 4.12 (valorAbsoluto 4.12))),
    TestLabel "test_absoluto_3" (TestCase (assertEqual "Valor Absoluto de 0 es: 0" 0 (valorAbsoluto 0))),

    TestLabel "test_bisiesto_1" (TestCase (assertEqual "1999 no es bisiesto" False (bisiesto 1999))),
    TestLabel "test_bisiesto_2" (TestCase (assertEqual "2004 es bisiesto" True (bisiesto 2004))),
    TestLabel "test_bisiesto_3" (TestCase (assertEqual "2000 es bisiesto" True (bisiesto 2000))),
    TestLabel "test_bisiesto_4" (TestCase (assertEqual "1500 no es bisiesto" False (bisiesto 1500))),
    TestLabel "test_bisiesto_5" (TestCase (assertEqual "14 no es bisiesto" False (bisiesto 14))),
    
    TestLabel "test_factorial_0" (TestCase (assertEqual "factorial 0: 1" 1 (factorial 0))),
    TestLabel "test_factorial_1" (TestCase (assertEqual "factorial 1: 1" 1 (factorial 1))),
    TestLabel "test_factorial_8" (TestCase (assertEqual "factorial 8: 40320" 40320 (factorial 8))),
    TestLabel "test_factorial_12" (TestCase (assertEqual "factorial 12: 479001600" 479001600 (factorial 12))),

    TestLabel "test_cantDivisoresPrimos_3" (TestCase (assertEqual "cantDivisoresPrimos del 3: 2" 2 (cantDivisoresPrimos 3))),
    TestLabel "test_cantDivisoresPrimos_10" (TestCase (assertEqual "cantDivisoresPrimos del 10: 3" 3 (cantDivisoresPrimos 10))),
    TestLabel "test_cantDivisoresPrimos_21" (TestCase (assertEqual "cantDivisoresPrimos del 21: 3" 3 (cantDivisoresPrimos 21))),
    TestLabel "test_cantDivisoresPrimos_682" (TestCase (assertEqual "cantDivisoresPrimos del 682: 4" 4 (cantDivisoresPrimos 682)))
    
    ]

tests_ej3 :: Test
tests_ej3 = TestList [
    TestLabel "test_inverso_0" (TestCase (assertEqual "Inverso multiplicativo 0: Nothing" Nothing (inverso 0))),
    TestLabel "test_inverso_1" (TestCase (assertEqual "Inverso multiplicativo 1: Just 1" (Just 1) (inverso 1))),
    TestLabel "test_inverso_5" (TestCase (assertEqual "Inverso multiplicativo 5: Just 0.2" (Just 0.2) (inverso 5))),
    TestLabel "test_inverso_5" (TestCase (assertEqual "Inverso multiplicativo -7: Just -0.14285715" (Just (-0.14285715)) (inverso (-7)))),

    TestLabel "test_aEntero_False" (TestCase (assertEqual "aEntero Right False: 0" 0 (aEntero (Right False)))),
    TestLabel "test_aEntero_True" (TestCase (assertEqual "aEntero Right True: 1" 1 (aEntero (Right True)))),
    TestLabel "test_aEntero_95" (TestCase (assertEqual "aEntero Left 95: 95" 95 (aEntero (Left 95))))

    ]

tests_ej4 :: Test
tests_ej4 = TestList [

    TestLabel "test_limpiar_susto_puerta" (TestCase (assertEqual "limpiar susto puerta: pera" "pera" (limpiar "susto" "puerta"))),
    TestLabel "test_limpiar_susto_puerta" (TestCase (assertEqual "limpiar '' puerta: puerta" "puerta" (limpiar "" "puerta"))),
    TestLabel "test_limpiar_susto_puerta" (TestCase (assertEqual "limpiar u puerta: perta" "perta" (limpiar "u" "puerta"))),

    TestLabel "test_difPromedio_[2,3,4]_[-1,0,1]" (TestCase (assertEqual "difPromedio [2,3,4]: [-1,0,1]" [-1, 0, 1] (difPromedio [2, 3, 4]))),
    TestLabel "test_difPromedio_[20,36,42]_[-12.666668,3.333332,9.333332]" (TestCase (assertEqual "difPromedio [20, 36, 42]: [-12.666668,3.333332,9.333332]" [-12.666668,3.333332,9.333332] (difPromedio [20, 36, 42]))),


    TestLabel "test_todosIguales_[]" (TestCase (assertEqual "todosIguales []: True" True (todosIguales []))),
    TestLabel "test_todosIguales_[3]" (TestCase (assertEqual "todosIguales [3]: True" True (todosIguales [3]))),

    TestLabel "test_todosIguales_[1,1,1]" (TestCase (assertEqual "todosIguales [1,1,1]: True" True (todosIguales [1,1,1]))),
    TestLabel "test_todosIguales_[1,0,1]" (TestCase (assertEqual "todosIguales [1,0,1]: False" False (todosIguales [1,0,1]))),
    TestLabel "test_todosIguales_[0,1,1]" (TestCase (assertEqual "todosIguales [0,1,1]: False" False (todosIguales [0,1,1]))),
    TestLabel "test_todosIguales_[0,0,1,1]" (TestCase (assertEqual "todosIguales [0,0,1,1]: False" False (todosIguales [0,0,1,1])))
    ]

tests_ej5 :: Test
tests_ej5 = TestList [

    TestLabel "test_vacioAB_Nil" (TestCase (assertEqual "vacioAB Nil: True" True (vacioAB Nil))),
    TestLabel "test_vacioAB_Bin Nil 4 Nil" (TestCase (assertEqual "vacioAB Bin Nil 4 Nil: False" False (vacioAB (Bin Nil 4 Nil)))),
    TestLabel "test_vacioAB_(Bin (Bin Nil 3 Nil) 4 (Bin Nil 2 Nil))" (TestCase (assertEqual "vacioAB Bin Nil 4 Nil: False" False (vacioAB (Bin (Bin Nil 3 Nil) 4 (Bin Nil 2 Nil))))),

    TestLabel "test_negacionAB_Nil" (TestCase (assertEqual "negacionAB Nil: Nil" Nil (negacionAB Nil))),
    TestLabel "test_negacionAB_Bin(Nil True Nil)" (TestCase (assertEqual "negacionAB_Bin(Nil True Nil): Bin(Nil False Nil)" (Bin Nil False Nil) (negacionAB (Bin Nil True Nil)))),
    TestLabel "test_negacionAB_Bin(Nil False Nil)" (TestCase (assertEqual "negacionAB_Bin(Nil False Nil): Bin(Nil True Nil)" (Bin Nil True Nil) (negacionAB (Bin Nil False Nil)))),
    TestLabel "test_negacionAB_(Bin (Bin Nil True Nil) False (Bin (Bin Nil True Nil) False (Bin Nil True Nil)))" (TestCase (assertEqual "negacionAB_Bin(Bin (Bin Nil True Nil) False (Bin (Bin Nil True Nil) False (Bin Nil True Nil))): (Bin (Bin Nil False Nil) True (Bin (Bin Nil False Nil) True (Bin Nil False Nil)))" (Bin (Bin Nil False Nil) True (Bin (Bin Nil False Nil) True (Bin Nil False Nil))) (negacionAB (Bin (Bin Nil True Nil) False (Bin (Bin Nil True Nil) False (Bin Nil True Nil)))))),


    TestLabel "test_productoAB_Nil" (TestCase (assertEqual "productoAB Nil: 1" 1 (productoAB Nil))),
    TestLabel "test_productoAB_Bin(Nil 5 Nil)" (TestCase (assertEqual "productoAB_Bin(Nil 5 Nil): 5" 5 (productoAB (Bin Nil 5 Nil)))),
    TestLabel "test_productoAB_Bin(Nil (-4) Nil)" (TestCase (assertEqual "productoAB_Bin(Nil (-4) Nil): -4" (-4) (productoAB (Bin Nil (-4) Nil)))),
    TestLabel "test_productoAB_(Bin (Bin Nil 7 Nil) 4 (Bin (Bin Nil 6 Nil) 2 (Bin Nil 3 Nil)))" (TestCase (assertEqual "productoAB_(Bin (Bin Nil 7 Nil) 4 (Bin (Bin Nil 6 Nil) 2 (Bin Nil 3 Nil))): 1008" 1008 (productoAB (Bin (Bin Nil 7 Nil) 4 (Bin (Bin Nil 6 Nil) 2 (Bin Nil 3 Nil))))))
    ]

main :: IO ()
main = do
    result2 <- runTestTT tests_ej2
    result3 <- runTestTT tests_ej3
    result4 <- runTestTT tests_ej4
    result5 <- runTestTT tests_ej5
    if failures result2 > 0 || failures result3 > 0 || failures result4 > 0 || failures result5 > 0 then Exit.exitFailure else Exit.exitSuccess
