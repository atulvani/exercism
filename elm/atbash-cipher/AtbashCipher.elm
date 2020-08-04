module AtbashCipher exposing (decode, encode)


encode : String -> String
encode plain =
    plain |> String.toLower |> cipherStr |> splitStrIntoWords 5


decode : String -> String
decode cipher =
    cipher |> cipherStr


splitStrIntoWords wordLength str =
    let
        left = String.left wordLength str
        rest = String.dropLeft wordLength str
    in
    case rest of
        "" -> left
        _ -> left ++ " " ++ (splitStrIntoWords wordLength rest)


cipherStr : String -> String
cipherStr str = str |> String.toList |> List.filter Char.isAlphaNum |> List.map (\c -> if (Char.isDigit c) then c else (cipherChar c)) |> String.fromList

cipherChar : Char -> Char
cipherChar char =
    let
        charCode = toFloat (Char.toCode char)
        mean = (toFloat ((Char.toCode 'a') + (Char.toCode 'z'))) / 2
        operation = if (charCode) >= mean then (-) else (+)
        delta = abs (((charCode) - mean) * 2)
    in
    Char.fromCode (floor ((operation) charCode (delta)))
