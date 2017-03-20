module Main where

import Sudoku


example1 =
    "003020600900305001001806400008102900700000008006708200002609500800203009005010300"


example2 =
    "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"


main :: IO ()
main =
    putStrLn (maybe "bad values" valuesToString (valuesFromString example1))
