module Sudoku
    ( Grid
    , parseGrid
    , solve
    , values
    )
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
            (\s -> ( s, unitsAll |> filter (elem s) ))
        |> Map.fromList


peersBySquare :: Map Square (Set Square)
peersBySquare =
    unitsBySquare
        |> Map.mapWithKey
            (\s units-> concat units |> Set.fromList |> Set.delete s)


-- Values

type Values =
    Map Square [ Char ]


toString :: Values -> String
toString values =
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


fromString :: String -> Maybe Values
fromString string =
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
                    (\values ( square, d ) -> assign square d values)
                    empty
        else
            Nothing


empty :: Values
empty =
    replicate (length squares) digits
        |> zip squares
        |> Map.fromList


{-
Eliminate all the other values (except d) from square and propagate.
-}
assign :: Square -> Char -> Values -> Maybe Values
assign square d values =
    values ! square
        |> filter (/= d)
        |> foldM (flip (eliminate square)) values


{-
Eliminate x from square. Propagate when square then contains only one value,
and also when x can only be placed in one other square. Return Nothing if a
contradiction is detected.
-}
eliminate :: Square -> Char -> Values -> Maybe Values
eliminate square x values =
    if not (elem x (values ! square)) then
        -- Already eliminated
        Just values
    else
        case values ! square |> filter (/= x) of
            -- Contradiction: removed last value
            [] ->
                Nothing

            value@(d : ds) ->
                values
                    |> Map.insert square value
                    -- (1) If square is reduced to one value d, then eliminate d from its peers.
                    |> (if ds == [] then
                            (\values ->
                                peersBySquare ! square
                                    |> foldM (\v s -> eliminate s d v) values
                            )
                        else
                            Just
                       )
                    -- (2) If a unit is reduced to only one square for the value x, then put it there.
                    >=> (\values ->
                            unitsBySquare ! square
                                |> foldM
                                    (\v unit ->
                                        case unit |> filter (\s -> elem x (v ! s)) of
                                            -- Contradiction: no place for this value
                                            [] ->
                                                Nothing

                                            s : [] ->
                                                v |> assign s x

                                            _ ->
                                                Just v

                                    )
                                    values
                        )


isSolved :: Values -> Bool
isSolved =
    Map.elems .> all (length .> (== 1))


{-
Using depth-first search and propagation, try all possible values.
-}
search :: Values -> Maybe Values
search values =
    if values |> isSolved then
        Just values
    else
        let
            -- Variable ordering: choose square having the _minimum remaining values_.
            ( _, square, value ) =
                values |> Map.filter (length .> (> 1)) |> Map.toList |> map (\( s, v ) -> ( length v, s, v )) |> minimum
        in
            -- Value ordering: no special order; choose d in given order.
            value
                |> mapMaybe
                    (\d -> assign square d values >>= search)
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


-- exposed

newtype Grid =
    Grid Values
    deriving (Eq)


instance Show Grid where
    show (Grid values) =
        toString values


parseGrid :: String -> Maybe Grid
parseGrid =
    fromString .> fmap Grid


solve :: Grid -> Maybe Grid
solve (Grid values) =
    search values |> fmap Grid


values :: Grid -> Map String String
values (Grid values) =
    values
