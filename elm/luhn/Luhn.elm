module Luhn exposing (valid)


valid : String -> Bool
valid input =
    let
        trimmedInput = String.replace " " "" input
        isLongEnough = (String.length trimmedInput) > 1
        hasOnlyDigits = List.all (Char.isDigit) (String.toList trimmedInput)
    in
    case isLongEnough && hasOnlyDigits of
        True ->
            let
                numList
                    = trimmedInput
                    |> String.toList
                    |> List.map (Char.toCode >> (-) (Char.toCode '0') >> abs)

                listWithAlternatesDoubled
                    = numList
                    |> List.reverse
                    |> List.indexedMap (\i -> \e -> if (modBy 2 i) /= 0 then e * 2 else e)
                    |> List.reverse

                digitList
                    = listWithAlternatesDoubled
                    |> List.map (\e -> if e > 9 then e - 9 else e)
            in
            digitList |> List.sum |> modBy 10 |> (==) 0
        False ->
            False
