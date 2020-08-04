module Anagram exposing (detect)

import List
import String


detect : String -> List String -> List String
detect word candidates =
    let
        lowerCasedSortedWord =
            getLowerCasedSortedWord word

        isDiffFromWord =
            String.toLower >> (/=) (String.toLower word)
    in
    List.filter
        (\c ->
            isDiffFromWord c && (getLowerCasedSortedWord c == lowerCasedSortedWord)
        )
        candidates


getLowerCasedSortedWord : String -> String
getLowerCasedSortedWord w =
    String.fromList (List.sort (String.toList (String.toLower w)))


areSameLengthWord : String -> String -> Bool
areSameLengthWord w1 w2 =
    String.length w1 == String.length w2
