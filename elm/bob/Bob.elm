module Bob exposing (hey)

import Char
import String


hey : String -> String
hey remark =
    case processRemark remark of
        IsBeingAskedQuestion ->
            "Sure."

        IsBeingYelledAt ->
            "Whoa, chill out!"

        IsBeingYelledQuestionAt ->
            "Calm down, I know what I'm doing!"

        IsJustAddressed ->
            "Fine. Be that way!"

        CanNotProcess ->
            "Whatever."


type ProcessedRemark
    = IsBeingAskedQuestion
    | IsBeingYelledAt
    | IsBeingYelledQuestionAt
    | IsJustAddressed
    | CanNotProcess


processRemark : String -> ProcessedRemark
processRemark remark =
    let
        remarkWithoutSpaces =
            String.trim remark

        isEmptyRemark =
            String.isEmpty remarkWithoutSpaces

        isInterrogatory =
            String.endsWith "?" remarkWithoutSpaces

        alphabetOnlyRemark =
            List.filter Char.isAlpha (String.toList remarkWithoutSpaces)

        isRude =
            not (List.isEmpty alphabetOnlyRemark)
            && List.all Char.isUpper alphabetOnlyRemark
    in
    if isEmptyRemark then
        IsJustAddressed

    else if isInterrogatory && isRude then
        IsBeingYelledQuestionAt

    else if isInterrogatory then
        IsBeingAskedQuestion

    else if isRude then
        IsBeingYelledAt

    else
        CanNotProcess
