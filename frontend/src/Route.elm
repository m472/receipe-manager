module Route exposing (..)

import Browser.Navigation as Nav
import Url.Parser exposing ((</>), (<?>))
import Url.Builder
import Url.Parser.Query
import Url.Builder as UB
import Url.Parser as UP exposing ((</>), (<?>))



-- URL PARSING


type Route
    = Overview (Maybe String)
    | ViewReceipe Int
    | EditReceipe Int
    | ImportReceipe


parse : UP.Parser (Route -> a) a
parse =
    UP.oneOf
        [ UP.map ViewReceipe (UP.s "receipe" </> UP.int)
        , UP.map EditReceipe
            (UP.s "receipe" </> UP.s "edit" </> UP.int)
        , UP.map ImportReceipe (UP.s "receipe" </> UP.s "import")
        , UP.map Overview (UP.top <?> Url.Parser.Query.string "tag")
        ]

-- URL BUILDING


load : Route -> Cmd a
load route =
    Nav.load (toString route)


toString : Route -> String
toString route =
    case route of
        Overview maybeTag ->
            UB.absolute []
            (
            case maybeTag of
                Just tag ->
                    [ Url.Builder.string "tag" tag]
                Nothing ->
                    []
                    )

        ViewReceipe id ->
            UB.absolute [ "receipe", String.fromInt id ] []

        EditReceipe id ->
            UB.absolute [ "receipe", "edit", String.fromInt id ] []

        ImportReceipe ->
            UB.absolute [ "receipe", "import" ] []

