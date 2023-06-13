module StyledElements exposing (..)

import Css exposing (..)
import Html.Styled exposing (..)


tagButton : List (Attribute msg) -> List (Html msg) -> Html msg
tagButton =
    styled a
        [ display inlineBlock
        , padding2 (ex 0.7) (ex 1.1)
        , margin (pt 2)
        , borderRadius (px 5)
        , backgroundColor (hex "888888")
        , textDecoration none
        , fontFamilies [ "Helvetica", "Arial" ]
        , color (hex "FFFFFF")
        , hover
            [ backgroundColor (hex "239e01")
            ]
        ]
