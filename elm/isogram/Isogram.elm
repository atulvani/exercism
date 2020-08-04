module Isogram exposing (isIsogram)

import Char
import String


isI5m : String -> List Char -> Bool
isI5m str invalidChars =
    case String.uncons str of
        Nothing ->
            True

        Just ( char, rest ) ->
            if List.member char invalidChars then
                False

            else if char == ' ' || char == '-' then
                isI5m rest invalidChars

            else
                isI5m rest (char :: invalidChars)


isIsogram : String -> Bool
isIsogram sentence =
    -- isI5m (String.toLower sentence) []
    sentence
        |> String.replace " " ""
        |> String.replace "-" ""
        |> String.toLower
        |> String.foldl
            (\c acc ->
                if Maybe.map (List.member c) acc == Just False then
                    Maybe.map ((::) c) acc

                else
                    Nothing
            )
            (Just [])
        |> Maybe.map (always True)
        |> Maybe.withDefault False
