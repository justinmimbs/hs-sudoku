module Sudoku
    where

import Data.Map (Map)
import Data.Set (Set)
import Flow
import qualified Data.Map as Map
import qualified Data.Set as Set


pair :: a -> a -> [ a ]
pair a b =
    [ a, b ]


cross :: [ a ] -> [ a ] -> [ [ a ] ]
cross as bs =
    [ pair a b | a <- as, b <- bs ]


groupsOf :: Int -> [ a ] -> [ [ a ] ]
groupsOf n list =
    case splitAt n list of
        ( [], _ ) ->
            []
        ( group, rest ) ->
            group : groupsOf n rest


--

digits :: [ Char ]
digits =
    "123456789"


letters :: [ Char ]
letters =
    "ABCDEFGHI"


type Square =
    [ Char ]

type Unit =
    [ Square ]

squares :: [ Square ]
squares =
    cross letters digits


unitsAll :: [ Unit ]
unitsAll =
    concat
        [ [ cross [a] digits | a <- letters ]
        , [ cross letters [b] | b <- digits ]
        , [ cross a b | a <- groupsOf 3 letters, b <- groupsOf 3 digits ]
        ]


unitsBySquare :: Map Square [ Unit ]
unitsBySquare =
    squares
        |> map
            (\s ->
                ( s, unitsAll |> filter (elem s))
            )
        |> Map.fromList


peersBySquare :: Map Square (Set Square)
peersBySquare =
    unitsBySquare
        |> Map.mapWithKey
            (\s units->
                concat units |> Set.fromList |> Set.delete s
            )


--

ex :: String
ex =
    "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"


type Values =
    Map Square [ Char ]
  --Map Square (Set Char)


valuesFromString :: String -> Maybe Values
valuesFromString string =
    let
        blanks =
            ".0"
        chars =
            string |> filter (\c -> elem c (blanks ++ digits))
    in
        if length chars == length squares then
            chars
                |> map (\c -> if elem c blanks then digits else [ c ])
                |> zip squares
                |> Map.fromList
                |> Just
        else
            Nothing
