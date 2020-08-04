module TwoFer exposing (twoFer)


twoFer : Maybe String -> String
twoFer name =
    let
        justName = case name of
            Just n -> n
            Nothing -> "you"
    in
        "One for " ++ justName ++ ", one for me."
