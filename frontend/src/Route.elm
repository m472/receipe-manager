module Route exposing (..)

import Browser.Navigation as Nav
import Url.Parser exposing ((</>), (<?>))



-- URL PARSING


type Route
    = Overview
    | ViewReceipe Int
    | EditReceipe Int


parse : Url.Parser.Parser (Route -> a) a
parse =
    Url.Parser.oneOf
        [ Url.Parser.map ViewReceipe (Url.Parser.s "receipe" </> Url.Parser.int)
        , Url.Parser.map EditReceipe
            (Url.Parser.s "receipe" </> Url.Parser.s "edit" </> Url.Parser.int)
        , Url.Parser.map Overview Url.Parser.top
        ]



-- URL BUILDING


load : Route -> Cmd a
load route =
    Nav.load
        (case route of
            Overview ->
                "/"

            ViewReceipe id ->
                "/receipe/" ++ String.fromInt id

            EditReceipe id ->
                "/receipe/edit/" ++ String.fromInt id
        )
