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

is_nothing : Maybe a -> Bool
is_nothing value =
    case value of
        Nothing -> True
        Just _ -> False

keep_just : List (Maybe a) -> List a
keep_just values =
    case values of
        [] -> []
        (x::xs) -> case x of
            Nothing -> keep_just xs
            Just val -> val::keep_just xs

lift : List (Maybe a) -> Maybe (List a)
lift values =
    case values of
        [] -> Just []
        (x::xs) -> 
            case (x, lift xs) of
                (Nothing, _) -> Nothing
                (_, Nothing) -> Nothing
                (Just xVal, Just xsVal) ->
                    Just (xVal::xsVal)
    
