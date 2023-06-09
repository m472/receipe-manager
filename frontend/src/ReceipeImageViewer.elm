module ReceipeImageViewer exposing (..)

import Array
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Receipe exposing (ReceipeID)
import Url.Builder


type Msg
    = NextImage
    | PrevImage



-- VIEW


viewImages : ReceipeID -> List Int -> Int -> Html Msg
viewImages receipeId imageIds currentImage =
    case Array.get currentImage (Array.fromList imageIds) of
        Just value ->
            div []
                [ button [ onClick PrevImage ] [ text "<" ]
                , img
                    [ src
                        (Url.Builder.absolute [ "receipe", "image" ]
                            [ Url.Builder.int "receipe_id" receipeId
                            , Url.Builder.int "image_id" value
                            ]
                        )
                    , width 500
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
