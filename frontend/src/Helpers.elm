module Helpers exposing (..)

import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Decimals(..), usLocale)


removeElementAt : Int -> List a -> List a
removeElementAt index list =
    List.take index list ++ List.drop (index + 1) list


viewMaybeFloat : Float -> Maybe Float -> String
viewMaybeFloat factor value =
    case value of
        Just floatValue ->
            format { usLocale | decimals = Max 1, thousandSeparator = "'" } (factor * floatValue)

        Nothing ->
            ""
