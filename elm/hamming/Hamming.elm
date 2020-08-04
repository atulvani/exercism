module Hamming exposing (distance)

import Result
import String


boolToInt : Bool -> Int
boolToInt v =
    if v == True then
        1

    else
        0


distance : String -> String -> Result String Int
distance left right =
    if String.length left == String.length right then
        Result.Ok
            (List.foldl (+)
                0
                (List.map2
                    -- not possible: ((==) >> not >> boolToInt)
                    -- not intuitive: (\charFromLeft charFromRight -> (charFromLeft == charFromRight) |> not |> boolToInt)
                    (\charFromLeft charFromRight ->
                        if charFromLeft == charFromRight then
                            0

                        else
                            1
                    )
                    (String.toList left)
                    (String.toList right)
                )
            )

    else
        Result.Err "left and right strands must be of equal length"


countDiffInStringsUsingRecursion : String -> String -> Int -> Int
countDiffInStringsUsingRecursion str1 str2 diffCount =
    case ( String.uncons str1, String.uncons str2 ) of
        ( Just ( char1, rest1 ), Just ( char2, rest2 ) ) ->
            if char1 == char2 then
                countDiffInStringsUsingRecursion rest1 rest2 diffCount

            else
                countDiffInStringsUsingRecursion rest1 rest2 (diffCount + 1)

        _ ->
            diffCount


distanceUsingRecursion : String -> String -> Result String Int
distanceUsingRecursion left right =
    if String.length left == String.length right then
        Result.Ok (countDiffInStringsUsingRecursion left right 0)

    else
        Result.Err "left and right strands must be of equal length"
