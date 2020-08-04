module TwelveDays exposing (recite)

import Array exposing (Array)


numString : Array String
numString =
    Array.fromList
        [ "first"
        , "second"
        , "third"
        , "fourth"
        , "fifth"
        , "sixth"
        , "seventh"
        , "eighth"
        , "ninth"
        , "tenth"
        , "eleventh"
        , "twelfth"
        ]



-- lyrics : List String
-- lyrics =
--     [ "Partridge in a Pear Tree."
--     , "two Turtle Doves, and a "
--     , "three French Hens, "
--     , "four Calling Birds, "
--     , "five Gold Rings, "
--     , "six Geese-a-Laying, "
--     , "seven Swans-a-Swimming, "
--     , "eight Maids-a-Milking, "
--     , "nine Ladies Dancing, "
--     , "ten Lords-a-Leaping, "
--     , "eleven Pipers Piping, "
--     , "twelve Drummers Drumming, "
--     ]
-- makeSentenceFromList : List String -> String
-- makeSentenceFromList xs =
--     xs |> List.reverse |> String.concat


lyricsWithoutPunctuation : List String
lyricsWithoutPunctuation =
    [ "a Partridge in a Pear Tree"
    , "two Turtle Doves"
    , "three French Hens"
    , "four Calling Birds"
    , "five Gold Rings"
    , "six Geese-a-Laying"
    , "seven Swans-a-Swimming"
    , "eight Maids-a-Milking"
    , "nine Ladies Dancing"
    , "ten Lords-a-Leaping"
    , "eleven Pipers Piping"
    , "twelve Drummers Drumming"
    ]


makeSentenceFromListWithoutPunctuation : List String -> String
makeSentenceFromListWithoutPunctuation xs =
    let
        suffix i =
            case i of
                0 ->
                    "."

                1 ->
                    ", and "

                _ ->
                    ", "
    in
    -- xs |> List.indexedMap (\i x -> x ++ suffix i) |> List.foldl (++) ""
    xs |> List.indexedMap (\i x -> x ++ suffix i) |> List.reverse |> String.concat


whichDay : Int -> String
whichDay day =
    "On the "
        ++ Maybe.withDefault "" (Array.get day numString)
        ++ " day of Christmas my true love gave to me: "
        -- ++ (if day == 0 then
        --         "a "
        --     else
        --         ""
        --    )
        ++ ""


getReciteItem : Int -> String
getReciteItem lineNumber =
    whichDay lineNumber
        -- ++ (lyrics |> List.take (lineNumber + 1) |> makeSentenceFromList)
        ++ (lyricsWithoutPunctuation |> List.take (lineNumber + 1) |> makeSentenceFromListWithoutPunctuation)


recite : Int -> Int -> List String
recite fromLine toLine =
    let
        lineNumbers =
            List.range (fromLine - 1) (toLine - 1)
    in
    List.map getReciteItem lineNumbers
