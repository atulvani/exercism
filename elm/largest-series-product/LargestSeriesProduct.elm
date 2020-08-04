module LargestSeriesProduct exposing (largestProduct)

import Array


chunk : Int -> List a -> List (List a)
chunk size list =
    let
        arr =
            Array.fromList list
    in
    arr
        |> Array.indexedMap
            (\i e ->
                if (i + 1) >= size then
                    Array.toList (Array.slice (i + 1 - size) (i + 1) arr)

                else
                    []
            )
        |> Array.filter (List.isEmpty >> not)
        |> Array.toList


maybeMax : Maybe Int -> Maybe Int -> Maybe Int
maybeMax a b =
    case ( a, b ) of
        ( Just m, Just n ) ->
            if m > n then
                Just m

            else
                Just n

        ( Just m, _ ) ->
            Just m

        ( _, Just n ) ->
            Just n

        _ ->
            Nothing


defaultToWhen defaultVal predicate a =
    if predicate a then
        defaultVal

    else
        a


largestProduct : Int -> String -> Maybe Int
largestProduct length series =
    -- beware, nested lists will consume more memory and will require more iterations
    if length == 0 then
        Just 1

    else
        series
            |> String.toList
            |> List.map (String.fromChar >> String.toInt)
            |> defaultToWhen [] (List.any ((==) Nothing))
            |> chunk length
            |> List.map (List.foldl (Maybe.map2 (*)) (Just 1))
            |> List.foldl maybeMax Nothing


splitListAcc i frstReversed scnd =
    case scnd of
        x :: xs ->
            if i <= 0 then
                ( List.reverse frstReversed, scnd )

            else
                splitListAcc (i - 1) (x :: frstReversed) xs

        _ ->
            ( List.reverse frstReversed, scnd )


splitList : Int -> List a -> ( List a, List a )
splitList index list =
    -- hope this is performant than take and drop
    splitListAcc index [] list


largestProductWithoutNestedLists : Int -> String -> Maybe Int
largestProductWithoutNestedLists length series =
    if length < 0 then
        Nothing

    else if length == 0 then
        Just 1

    else if length > String.length series then
        Nothing

    else
        let
            digitList =
                series
                    |> String.toList
                    |> List.map (String.fromChar >> String.toInt)
        in
        -- can do this check above and save one level of nesting
        -- but why iterate if length alone can predict the result
        if List.any ((==) Nothing) digitList then
            Nothing

        else
            digitList
                |> List.foldl
                    (\n prodList ->
                        let
                            ( firstIrrelevant, lastRelevant ) =
                                splitList (List.length prodList - length + 1) prodList
                        in
                        firstIrrelevant
                            ++ List.map (Maybe.map2 (*) n) lastRelevant
                            ++ [ n ]
                    )
                    (List.repeat (length - 1) Nothing)
                |> List.reverse
                |> List.drop (length - 1)
                |> List.map (Maybe.withDefault 0)
                |> List.maximum
