module Pangram exposing (isPangram, isPangramUsingRecursion)

import Set exposing (Set)
import String


insertAlphaInSet : Char -> Set Char -> Set Char
insertAlphaInSet char set =
    if Char.isAlpha char then
        Set.insert char set

    else
        set


isPangram : String -> Bool
isPangram sentence =
    if String.length sentence >= 26 then
        sentence
            |> String.toLower
            -- not optimized for huge strings, doesn't allow the break in middle of fold
            |> String.foldl insertAlphaInSet Set.empty
            |> Set.size
            |> (==) 26

    else
        False


fillSetWithCharsFromString : String -> Set Char -> Set Char
fillSetWithCharsFromString str set =
    if Set.size set < 26 then
        case String.uncons str of
            Just ( char, rest ) ->
                if Char.isAlpha char then
                    fillSetWithCharsFromString rest (Set.insert char set)

                else
                    fillSetWithCharsFromString rest set

            _ ->
                set

    else
        set


isPangramUsingRecursion : String -> Bool
isPangramUsingRecursion sentence =
    if String.length sentence >= 26 then
        26 == Set.size (fillSetWithCharsFromString (String.toLower sentence) Set.empty)

    else
        False
