module AllYourBase exposing (rebase)


isAnyNegative : List Int -> Bool
isAnyNegative =
    List.any (\x -> x < 0)


isAnyGreaterThenOrEqualTo : Int -> List Int -> Bool
isAnyGreaterThenOrEqualTo n xs =
    List.any (\x -> x >= n) xs


numberToDigitList : Int -> List Int
numberToDigitList =
    String.fromInt >> String.toList >> List.map String.fromChar >> List.filterMap String.toInt


toBaseTen : Int -> List Int -> Maybe (List Int)
toBaseTen inBase digits =
    if inBase <= 1 then
        Nothing

    else if List.isEmpty digits then
        Nothing

    else if isAnyNegative digits then
        Nothing

    else if isAnyGreaterThenOrEqualTo inBase digits then
        Nothing

    else
        digits
            |> List.reverse
            |> List.indexedMap (\i d -> d * (inBase ^ i))
            |> List.sum
            |> numberToDigitList
            |> Just


digitListToNumber : List Int -> Int
digitListToNumber =
    List.map String.fromInt >> List.foldr (++) "" >> String.toInt >> Maybe.withDefault 0


countInList : List a -> a -> Int
countInList list item =
    List.length (List.filter ((==) item) list)


calcDigitSeqCountList : Int -> Int -> List Int
calcDigitSeqCountList outBase number =
    -- 67 to base8 => (8^2)*1 + (8^1)*0 + (8^0)*3 => [2, 0, 0, 0]
    -- 67 to base7 => (7^2)*1 + (7^1)*2 + (7^0)*4 => [2, 1, 1, 0, 0, 0, 0]
    let
        mostSignificantDigit =
            floor (logBase (toFloat outBase) (toFloat number))

        rest =
            number - (outBase ^ mostSignificantDigit)
    in
    if rest > 0 then
        mostSignificantDigit :: calcDigitSeqCountList outBase rest

    else
        [ mostSignificantDigit ]


fromBaseTen : Int -> List Int -> Maybe (List Int)
fromBaseTen outBase digits =
    if outBase <= 1 then
        Nothing

    else if List.isEmpty digits then
        Nothing

    else if isAnyNegative digits then
        Nothing

    else
        let
            digitSeqCount =
                calcDigitSeqCountList outBase (digitListToNumber digits)

            maxIndex =
                Maybe.withDefault -1 (List.head digitSeqCount)

            indexRange =
                List.reverse (List.range 0 maxIndex)
        in
        -- [2, 0, 0, 0] for base8 -> 103
        -- [2, 1, 1, 0, 0, 0, 0] for base7 -> 124
        Just (List.map (\i -> countInList digitSeqCount i) indexRange)


rebase : Int -> List Int -> Int -> Maybe (List Int)
rebase inBase digits outBase =
    if List.all ((==) 0) digits then
        Nothing

    else
        digits |> toBaseTen inBase |> Maybe.andThen (fromBaseTen outBase)
