module Route exposing (..)

import Browser.Navigation as Nav
import Url.Parser exposing ((</>), (<?>))
import Url.Builder
import Url.Parser.Query



-- URL PARSING


type Route
    = Overview (Maybe String)
    | ViewReceipe Int
    | EditReceipe Int


parse : Url.Parser.Parser (Route -> a) a
parse =
    Url.Parser.oneOf
        [ Url.Parser.map ViewReceipe (Url.Parser.s "receipe" </> Url.Parser.int)
        , Url.Parser.map EditReceipe
            (Url.Parser.s "receipe" </> Url.Parser.s "edit" </> Url.Parser.int)
        , Url.Parser.map Overview (Url.Parser.top <?> Url.Parser.Query.string "tag")
        ]



-- URL BUILDING


load : Route -> Cmd a
load route =
    Nav.load
        (case route of
            Overview maybeTag ->
                Url.Builder.absolute ["/"]
                (
                case maybeTag of
                    Just tag ->
                        [Url.Builder.string "tag" tag]
                    Nothing ->
                        []
                        )


            ViewReceipe id ->
                "/receipe/" ++ String.fromInt id

            EditReceipe id ->
                "/receipe/edit/" ++ String.fromInt id
        )
