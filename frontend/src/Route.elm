module Route exposing (..)

import Browser.Navigation as Nav
import Url.Builder as UB
import Url.Parser as UP exposing ((</>), (<?>))



-- URL PARSING


type Route
    = Overview
    | ViewReceipe Int
    | EditReceipe Int
    | ImportReceipe
    | ViewCategory String


parse : UP.Parser (Route -> a) a
parse =
    UP.oneOf
        [ UP.map ViewReceipe (UP.s "receipe" </> UP.int)
        , UP.map EditReceipe
            (UP.s "receipe" </> UP.s "edit" </> UP.int)
        , UP.map ImportReceipe (UP.s "receipe" </> UP.s "import")
        , UP.map Overview UP.top
        , UP.map ViewCategory (UP.s "category" </> UP.string)
        ]

-- URL BUILDING


load : Route -> Cmd a
load route =
    Nav.load (toString route)


toString : Route -> String
toString route =
    case route of
        Overview ->
            UB.absolute [ "/" ] []

        ViewReceipe id ->
            UB.absolute [ "receipe", String.fromInt id ] []

        EditReceipe id ->
            UB.absolute [ "receipe", "edit", String.fromInt id ] []

        ImportReceipe ->
            UB.absolute [ "receipe", "import" ] []

        ViewCategory tag ->
            UB.absolute ["category", tag] []
