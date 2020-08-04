module SpaceAge exposing (Planet(..), ageOn)


type Planet
    = Mercury
    | Venus
    | Earth
    | Mars
    | Jupiter
    | Saturn
    | Uranus
    | Neptune


toEarthYears : Float -> Float
toEarthYears seconds =
    seconds / 31557600


toPlanetYears : Planet -> Float -> Float
toPlanetYears planet earthYears =
    case planet of
        Mercury ->
            earthYears / 0.2408467

        Venus ->
            earthYears / 0.61519726

        Earth ->
            earthYears / 1

        Mars ->
            earthYears / 1.8808158

        Jupiter ->
            earthYears / 11.862615

        Saturn ->
            earthYears / 29.447498

        Uranus ->
            earthYears / 84.016846

        Neptune ->
            earthYears / 164.79132


ageOn : Planet -> Float -> Float
ageOn planet seconds =
    seconds |> toEarthYears |> toPlanetYears planet
