module Sudoku
    where

import Data.List (unlines, unwords, intercalate)
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


-- squares, units, peers

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
        , [ cross as bs | as <- groupsOf 3 letters, bs <- groupsOf 3 digits ]
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


-- Values

type Values =
    Map Square [ Char ]


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


valuesToString :: Values -> String
valuesToString values =
    let
        list =
            Map.elems values

        len =
            list |> map length |> maximum
    in
        list
            |> map (padLeft len ' ')
            |> groupsOf 9
            |> map (groupsOf 3 .> map unwords .> intercalate " | ")
            |> groupsOf 3
            |> intercalate [ replicate 3 (replicate (len * 3 + 2) '-') |> intercalate "-+-" ]
            |> unlines


-- utilities

groupsOf :: Int -> [ a ] -> [ [ a ] ]
groupsOf n list =
    case splitAt n list of
        ( [], _ ) ->
            []
        ( group, rest ) ->
            group : groupsOf n rest


padLeft :: Int -> a -> [ a ] -> [ a ]
padLeft n x list =
    replicate (max 0 (n - length list)) x ++ list
