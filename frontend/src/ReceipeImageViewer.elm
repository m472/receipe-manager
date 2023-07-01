module ReceipeImageViewer exposing (..)

import Api
import Array
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Receipe exposing (ReceipeID)


type Msg
    = NextImage
    | PrevImage



-- VIEW


viewImages : ReceipeID -> List Int -> Int -> Html Msg
viewImages receipeId imageIds currentImage =
    case Array.get currentImage (Array.fromList imageIds) of
        Just _ ->
            div [ class "img-viewer" ]
                [ button
                    [ onClick PrevImage ]
                    [ text "<" ]
                , img [ src (Api.ReceipeImage receipeId currentImage |> Api.toString) ]
                    []
                , button
                    [ onClick NextImage ]
                    [ text ">" ]
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
