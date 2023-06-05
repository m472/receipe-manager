module ReceipeImageViewer exposing (..)

import Array
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Receipe exposing (Receipe)
import Url.Builder


type Msg
    = NextImage
    | PrevImage



-- VIEW


viewImages : Receipe -> Int -> Html Msg
viewImages receipe currentImage =
    case Array.get currentImage (Array.fromList receipe.image_ids) of
        Just value ->
            div []
                [ button [ onClick PrevImage ] [ text "<" ]
                , img
                    [ src
                        (Url.Builder.absolute [ "receipe", "image" ]
                            [ Url.Builder.int "receipe_id" receipe.id
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


update : Msg -> Receipe -> Int -> Int
update msg receipe currentImage =
    let
        imgCount =
            List.length receipe.image_ids
    in
    case msg of
        NextImage ->
            modBy imgCount (currentImage + 1)

        PrevImage ->
            if currentImage > 0 then
                currentImage - 1

            else
                imgCount - 1
