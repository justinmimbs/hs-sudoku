module Main
    ( main
    , spec
    )
    where

import Test.Hspec
import qualified Data.Map as Map
import qualified Sudoku


main :: IO ()
main =
    hspec spec


spec :: Spec
spec = do
    describe "Sudoku.parseGrid" $
        it "returns `Just Sudoku.Grid` on successful parse; otherwise `Nothing`" $ do
            shouldBe
                (Map.size . Sudoku.values <$> Sudoku.parseGrid "003020600900305001001806400008102900700000008006708200002609500800203009005010300")
                (Just 81)

            shouldBe
                (Sudoku.parseGrid "03020600900305001001806400008102900700000008006708200002609500800203009005010300")
                Nothing

            shouldBe
                (Sudoku.parseGrid "303020600900305001001806400008102900700000008006708200002609500800203009005010300")
                Nothing

    describe "Sudoku.solve" $
        it "solves as exepected" $
            shouldBe
                (Map.elems . Sudoku.values <$> (Sudoku.parseGrid "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......" >>= Sudoku.solve))
                (Just
                    [ "4","1","7","3","6","9","8","2","5"
                    , "6","3","2","1","5","8","9","4","7"
                    , "9","5","8","7","2","4","3","1","6"
                    , "8","2","5","4","3","7","1","6","9"
                    , "7","9","1","5","8","6","4","3","2"
                    , "3","4","6","9","1","2","7","5","8"
                    , "2","8","9","6","4","3","5","7","1"
                    , "5","7","3","2","9","1","6","8","4"
                    , "1","6","4","8","7","5","2","9","3"
                    ]
                )

    describe "Sudoku.Grid" $
        it "is an instance of Show" $
            shouldBe
                (maybe "" show $ Sudoku.parseGrid "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......" >>= Sudoku.solve)
                (unlines
                    [ "4 1 7 | 3 6 9 | 8 2 5"
                    , "6 3 2 | 1 5 8 | 9 4 7"
                    , "9 5 8 | 7 2 4 | 3 1 6"
                    , "------+-------+------"
                    , "8 2 5 | 4 3 7 | 1 6 9"
                    , "7 9 1 | 5 8 6 | 4 3 2"
                    , "3 4 6 | 9 1 2 | 7 5 8"
                    , "------+-------+------"
                    , "2 8 9 | 6 4 3 | 5 7 1"
                    , "5 7 3 | 2 9 1 | 6 8 4"
                    , "1 6 4 | 8 7 5 | 2 9 3"
                    ]
                )
