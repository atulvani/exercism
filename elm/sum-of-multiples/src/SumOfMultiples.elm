module SumOfMultiples exposing (sumOfMultiples)

isDivisibleBy : Int -> Int -> Bool
isDivisibleBy number divisor =
    (modBy divisor number) == 0


isDivisibleByAnyOf : List Int -> Int -> Bool
isDivisibleByAnyOf divisors number =
    List.any (isDivisibleBy number) divisors


listOfMultiples : List Int -> Int -> List Int
listOfMultiples divisors limit =
    (limit - 1)
    |> List.range 2
    |> List.filter (isDivisibleByAnyOf divisors)


sumOfMultiples : List Int -> Int -> Int
sumOfMultiples divisors limit =
    listOfMultiples divisors limit |> List.sum
