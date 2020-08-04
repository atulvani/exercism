module Triangle exposing (Triangle(..), triangleKind)


import Set

type Triangle
    = Equilateral
    | Isosceles
    | Scalene


type alias TriangleEdges number = (number, number, number)


makeTriangleEdges : number -> number -> number -> Result String (TriangleEdges number)
makeTriangleEdges x y z =
    let
        h = -- hypotenuse
            if (x >= y && x >= z) then x
            else if (y >= x && y >= z) then y
            else z

        a =
            if (x <= y && x <= z) then x
            else if (y <= x && y <= z) then y
            else z
            
        b =
            if (Set.fromList [y, z] == Set.fromList [a, h]) then x
            else if (Set.fromList [x, z] == Set.fromList [a, h]) then y
            else z
    in
    if (a <= 0 || b <= 0) then
        Err "Invalid lengths"
    else if (a + b <= h) then
        Err "Violates inequality"
    else
        Ok (a, b, h)


triangleKind : number -> number -> number -> Result String Triangle
triangleKind x y z =
    makeTriangleEdges x y z
        |> Result.andThen (\(a, b, h) ->
                case (a == b, b == h, h == a) of
                    (True, True, _) -> Ok Equilateral
                    (False, False, False) -> Ok Scalene
                    _ -> Ok Isosceles
            )
