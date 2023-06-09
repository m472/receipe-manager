module Importer exposing (..)

import Api
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Receipe
import Route



-- MODEL


type Model
    = EnterUrl String
    | Loading


type Msg
    = ImportResult (Result Http.Error Receipe.Receipe)
    | Import
    | UrlUpdated String



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        EnterUrl url ->
            div []
                [ h1 [] [ text "Rezept-Import" ]
                , text "Rezept-Url:"
                , input [ type_ "url", value url, onInput UrlUpdated ] []
                , button [ onClick Import ] [ text "importieren" ]
                ]

        Loading ->
            text "Rezept wird importiert..."


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( EnterUrl _, UrlUpdated url ) ->
            ( EnterUrl url, Cmd.none )

        ( EnterUrl url, Import ) ->
            ( Loading, getImportReceipe url )

        ( Loading, ImportResult (Ok receipe) ) ->
            ( model, Route.load (Route.EditReceipe receipe.id))

        -- ignore messages from non-matching states
        ( _, _ ) ->
            ( model, Cmd.none )



-- HTTP


getImportReceipe : String -> Cmd Msg
getImportReceipe url =
    Api.post
        { endpoint = Api.ImportReceipe url
        , expect = Http.expectJson ImportResult Receipe.decoder
        , body = Http.emptyBody
        }
