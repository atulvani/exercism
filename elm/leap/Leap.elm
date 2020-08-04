module Leap exposing (isLeapYear)


isLeapYear : Int -> Bool
isLeapYear year =
    case ( isDivBy 400 year, isDivBy 100 year, isDivBy 4 year ) of
        ( True, _, _ ) ->
            True

        ( _, True, _ ) ->
            False

        ( _, _, True ) ->
            True

        _ ->
            False


isDivBy y x =
    modBy y x == 0
