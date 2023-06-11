module ReceipeImageViewer exposing (..)

import Api
import Array
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Receipe exposing (ReceipeID)
import Url.Builder
import Css exposing (..)


type Msg
    = NextImage
    | PrevImage



-- VIEW


viewImages : ReceipeID -> List Int -> Int -> Html Msg
viewImages receipeId imageIds currentImage =
    case Array.get currentImage (Array.fromList imageIds) of
        Just value ->
            div [css [displayFlex, margin2 (pt 20) (pt 0)] ]
                [ button [ onClick PrevImage ] [ text "<" ]
                , img
                    [ src (Api.ReceipeImage receipeId currentImage |> Api.toString)
                    , Html.Styled.Attributes.width 500
                    ]
                    []
                , button [ onClick NextImage ] [ text ">" ]
                ]

        Nothing ->
            div [] [ text "Keine Bilder gefunden" ]



-- UPDATE


update : Msg -> List Int -> Int -> Int
update msg image_ids currentImage =
    let
        imgCount =
            List.length image_ids
    in
    case msg of
        NextImage ->
            modBy imgCount (currentImage + 1)

        PrevImage ->
            if currentImage > 0 then
                currentImage - 1

            else
                imgCount - 1
