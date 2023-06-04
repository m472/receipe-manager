module Helpers exposing (..)

import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Decimals(..), usLocale)


removeElementAt : Int -> List a -> List a
removeElementAt index list =
    List.take index list ++ List.drop (index + 1) list


updateElementAt : Int -> (a -> a) -> List a -> List a
updateElementAt index updateFunc =
    List.indexedMap
        (\i elem ->
            if i == index then
                updateFunc elem

            else
                elem
        )


viewMaybeFloat : Float -> Maybe Float -> String
viewMaybeFloat factor value =
    case value of
        Just floatValue ->
            format { usLocale | decimals = Max 1, thousandSeparator = "'" } (factor * floatValue)

        Nothing ->
            ""
