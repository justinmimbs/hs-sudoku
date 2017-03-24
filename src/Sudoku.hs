module Sudoku
    where

import Control.Monad (foldM, (>=>))
import Data.List (unlines, unwords, intercalate)
import Data.Map (Map, (!))
import Data.Maybe (fromMaybe, mapMaybe, listToMaybe)
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
                |> zip squares
                |> filter (\( _, char ) -> not (elem char blanks))
                |> foldM
                    (\values ( square, d ) -> valuesAssign square d values)
                    valuesEmpty
        else
            Nothing


valuesEmpty :: Values
valuesEmpty =
    replicate (length squares) digits
        |> zip squares
        |> Map.fromList


valuesAssign :: Square -> Char -> Values -> Maybe Values
valuesAssign square d values =
    Map.lookup square values
        |> maybe [] (filter (/= d))
        |> foldM (flip (valuesEliminate square)) values


valuesEliminate :: Square -> Char -> Values -> Maybe Values
valuesEliminate square x values =
    if not (elem x (values ! square)) then
        Just values
    else
        case values ! square |> filter (/= x) of
            [] ->
                Nothing

            value@(d : ds) ->
                values
                    |> Map.insert square value
                    |> (if ds == [] then
                            (\values ->
                                peersBySquare ! square
                                    |> foldM (\v s -> valuesEliminate s d v) values
                            )
                        else
                            Just
                       )
                    >=> (\values ->
                            unitsBySquare ! square
                                |> foldM
                                    (\v unit ->
                                        case unit |> filter (\s -> elem x (v ! s)) of
                                            [] ->
                                                Nothing

                                            s : [] ->
                                                v |> valuesAssign s x

                                            _ ->
                                                Just v

                                    )
                                    values
                        )


isSolved :: Values -> Bool
isSolved =
    Map.elems .> all (length .> (== 1))


valuesSearch :: Values -> Maybe Values
valuesSearch values =
    if values |> isSolved then
        Just values
    else
        let
            -- variable ordering: choose square having the _minimum remaining values_
            ( _, square, ds ) =
                values |> Map.filter (length .> (> 1)) |> Map.toList |> map (\( s, v ) -> ( length v, s, v )) |> minimum
        in
            -- value ordering: no special order; choose d in order of ds
            ds
                |> mapMaybe
                    (\d -> valuesAssign square d values >>= valuesSearch)
                |> listToMaybe


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
