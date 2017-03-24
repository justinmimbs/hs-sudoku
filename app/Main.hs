module Main where

import Control.Monad ((>=>))
import Flow
import System.Environment (getArgs)
import qualified Sudoku


main :: IO ()
main =
    getArgs >>=
        map ((Sudoku.parseGrid >=> Sudoku.solve) .> maybe "invalid grid" show)
            .> unlines
            .> putStrLn
