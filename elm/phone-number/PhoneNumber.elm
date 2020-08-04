module PhoneNumber exposing (getNumber)


flip : (a -> b -> c) -> (b -> a -> c)
flip func a b =
    func b a


canIgnoreChar : Char -> Bool
canIgnoreChar =
    flip List.member [ ' ', '.', '-', '(', ')', '+' ]


doesCharAtIndexSatisfy : Int -> (Char -> Bool) -> String -> Bool
doesCharAtIndexSatisfy index predicate str =
    str
        |> String.slice index (index + 1)
        |> String.toList
        |> List.head
        |> Maybe.map predicate
        |> Maybe.withDefault False


isChar0Or1 : Char -> Bool
isChar0Or1 =
    flip List.member [ '0', '1' ]


getNumber : String -> Maybe String
getNumber phoneNumber =
    let
        filteredPhoneNumber =
            phoneNumber
                |> filterStringWithFold Char.isDigit canIgnoreChar
                -- |> filterStringRecursive Char.isDigit canIgnoreChar ""
                |> (\pn ->
                        if (String.length pn == 11) && String.startsWith "1" pn then
                            String.dropLeft 1 pn

                        else
                            pn
                   )
    in
    if String.length filteredPhoneNumber /= 10 then
        Nothing

    else if doesCharAtIndexSatisfy 0 isChar0Or1 filteredPhoneNumber then
        Nothing

    else if doesCharAtIndexSatisfy 3 isChar0Or1 filteredPhoneNumber then
        Nothing

    else
        Just filteredPhoneNumber


filterStringWithFold : (Char -> Bool) -> (Char -> Bool) -> String -> String
filterStringWithFold shouldAccept shouldIgnore str =
    str
        |> String.foldl
            (\c reversedFilteredStr ->
                if shouldAccept c then
                    -- assuming cons is performant than ++
                    Maybe.map (String.cons c) reversedFilteredStr

                else if shouldIgnore c then
                    reversedFilteredStr

                else
                    Nothing
            )
            (Just "")
        |> Maybe.withDefault ""
        |> String.reverse


filterStringRecursive : (Char -> Bool) -> (Char -> Bool) -> String -> String -> String
filterStringRecursive shouldAccept shouldIgnore result str =
    case String.uncons str of
        Just ( char, rest ) ->
            if shouldAccept char then
                filterStringRecursive shouldAccept shouldIgnore (result ++ String.fromChar char) rest

            else if shouldIgnore char then
                filterStringRecursive shouldAccept shouldIgnore result rest

            else
                result

        _ ->
            result
