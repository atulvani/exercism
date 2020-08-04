module Series exposing (slices)


slices : Int -> String -> Result String (List (List Int))
slices size input =
    let
        inputSize =
            String.length input
    in
    case ( compareNums size 0, compareNums inputSize 0, compareNums size inputSize ) of
        ( Gt, Gt, Lt ) ->
            Ok (List.map getNumListFromNumString (getChunks 0 size input))

        ( Gt, Gt, Eq ) ->
            Ok [ getNumListFromNumString input ]

        ( Lt, _, _ ) ->
            Err "slice length cannot be negative"

        ( Eq, _, _ ) ->
            Err "slice length cannot be zero"

        ( _, Eq, _ ) ->
            Err "series cannot be empty"

        ( _, _, Gt ) ->
            Err "slice length cannot be greater than series length"

        _ ->
            Err "THIS SHALL NEVER HAPPEN"


getChunks : Int -> Int -> String -> List String
getChunks fromIndex size str =
    let
        toIndex =
            fromIndex + size
    in
    case compareNums toIndex (String.length str) of
        Lt ->
            String.slice fromIndex toIndex str :: getChunks (fromIndex + 1) size str

        Eq ->
            [ String.slice fromIndex toIndex str ]

        Gt ->
            []


getNumListFromNumString : String -> List Int
getNumListFromNumString str =
    str |> String.toList |> List.map String.fromChar |> List.filterMap String.toInt


type NumCompResult
    = Lt
    | Eq
    | Gt


compareNums : Int -> Int -> NumCompResult
compareNums n1 n2 =
    case ( n1 < n2, n1 == n2 ) of
        ( True, _ ) ->
            Lt

        ( _, True ) ->
            Eq

        _ ->
            Gt
