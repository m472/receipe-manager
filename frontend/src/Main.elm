module Main exposing (..)

import Api
import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Importer
import Json.Decode as JD
import Platform.Cmd as Cmd
import Receipe
import ReceipeEditor
import ReceipeViewer
import Route exposing (Route(..))
import Url
import Url.Builder as UB
import Url.Parser exposing ((</>), (<?>))



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type Mode
    = Display
    | Edit


type ModelContent
    = Failure String
    | Loading String
    | ReceipeViewer ReceipeViewer.Model
    | ReceipeEditor ReceipeEditor.Model
    | ViewReceipeList (List ReceipePreview)
    | ReceipeImporter Importer.Model


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , content : ModelContent
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        ( content, cmd ) =
            onUrlChange url
    in
    ( Model key url content, cmd )


type alias ReceipePreview =
    { id : Int
    , title : String
    , image_ids : List Int
    }



-- UPDATE


type Msg
    = GotReceipe (Result Http.Error Receipe.Receipe)
    | GotReceipeForEdit (Result Http.Error Receipe.Receipe)
    | GotNewReceipe (Result Http.Error Receipe.Receipe)
    | GotReceipeList (Result Http.Error (List ReceipePreview))
    | RoutedEditMsg ReceipeEditor.Model ReceipeEditor.Msg
    | RoutedImportMsg Importer.Model Importer.Msg
    | RoutedReceipeMsg ReceipeViewer.Model ReceipeViewer.Msg
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | CreateReceipe
    | OpenReceipeImporter


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotReceipe result ->
            case result of
                Ok receipe ->
                    ( { model
                        | content = ReceipeViewer (ReceipeViewer.modelFromReceipe receipe "")
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | content = Failure "Rezept laden fehlgeschlagen" }, Cmd.none )

        GotReceipeForEdit result ->
            case result of
                Ok receipe ->
                    ( { model | content = ReceipeEditor { currentImage = 0, receipe = receipe, errorMessage = "" } }, Cmd.none )

                Err _ ->
                    ( { model 
                    | content = Failure "Rezept zum Editieren laden fehlgeschlagen" }, Cmd.none )

        GotNewReceipe result ->
            case result of
                Ok receipe ->
                    ( { model | content = ReceipeViewer (ReceipeViewer.modelFromReceipe receipe "") }, Cmd.none )

                Err _ ->
                    ( { model | content = Failure "Rezept konnte nicht geladen werden" }, Cmd.none )

        GotReceipeList result ->
            case result of
                Ok receipes ->
                    ( { model | content = ViewReceipeList receipes }, Cmd.none )

                Err _ ->
                    ( { model | content = Failure "Rezeptliste konnte nicht geladen werden" }, Cmd.none )

        RoutedEditMsg subModel childMsg ->
            let
                ( updatedContent, cmd ) =
                    ReceipeEditor.updateReceipe childMsg subModel
            in
            ( { model
                | content = ReceipeEditor updatedContent
              }
            , Cmd.map (RoutedEditMsg subModel) cmd
            )

        RoutedReceipeMsg scaled_receipe childMsg ->
            let
                ( updatedReceipe, cmd ) =
                    ReceipeViewer.update childMsg scaled_receipe
            in
            ( { model | content = ReceipeViewer updatedReceipe }
            , Cmd.map (RoutedReceipeMsg updatedReceipe) cmd
            )

        RoutedImportMsg subModel childMsg ->
            let
                ( updatedModel, cmd ) =
                    Importer.update childMsg subModel
            in
            ( { model | content = ReceipeImporter updatedModel }
            , Cmd.map (RoutedImportMsg updatedModel) cmd
            )

        UrlChanged url ->
            let
                ( content, cmd ) =
                    onUrlChange url
            in
            ( { model | content = content }, cmd )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        CreateReceipe ->
            ( { model | content = Loading "neues Rezept" }, getNewReceipe )

        OpenReceipeImporter ->
            ( { model | content = Loading "Rezept Importer" }, Route.load Route.ImportReceipe)


onUrlChange : Url.Url -> ( ModelContent, Cmd Msg )
onUrlChange url =
    case Url.Parser.parse Route.parse url of
        Just (Route.ViewReceipe id) ->
            ( Loading ("Receipe " ++ String.fromInt id), getReceipe GotReceipe id )

        Just (Route.EditReceipe id) ->
            ( Loading ("ReceipeEditor" ++ String.fromInt id), getReceipe GotReceipeForEdit id )

        Just Route.Overview ->
            ( Loading "Overview", getReceipeList )

        Just Route.ImportReceipe ->
            ( ReceipeImporter (Importer.EnterUrl ""), Cmd.none )

        Nothing ->
            ( Failure "Url konnte nicht geparst werden", Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    Browser.Document
        "Receipe Manager"
        [ main_ []
            [ case model.content of
                Failure errorMsg ->
                    text ("Fehler: " ++ errorMsg)

                Loading msg ->
                    text ("loading " ++ msg ++ " ...")

                ReceipeViewer receipeViewerModel ->
                    Html.map (RoutedReceipeMsg receipeViewerModel)
                        (ReceipeViewer.view receipeViewerModel)

                ReceipeEditor receipeEditorModel ->
                    Html.map (RoutedEditMsg receipeEditorModel)
                        (ReceipeEditor.view receipeEditorModel)

                ViewReceipeList receipeList ->
                    viewReceipeList receipeList

                ReceipeImporter importerModel ->
                    Html.map (RoutedImportMsg importerModel)
                        (Importer.view importerModel)
            ]
        ]


viewReceipeList : List ReceipePreview -> Html Msg
viewReceipeList receipeList =
    div []
        [ h1 [] [ text "Rezepte-Übersicht" ]
        , button [ onClick CreateReceipe ] [ text "neues Rezept" ]
        , button [ onClick OpenReceipeImporter ] [ text "Rezept importieren" ]
        , ul []
            (List.map
                (\receipe ->
                    li []
                        [ a [ href (Route.ViewReceipe receipe.id |> Route.toString) ]
                            [ text
                                (case receipe.title of
                                    "" ->
                                        "Untitled"

                                    title ->
                                        title
                                )
                            ]
                        ]
                )
                receipeList
            )
        ]



-- HTTP


getReceipeList : Cmd Msg
getReceipeList =
    Http.get
        { url = Api.ReceipeList |> Api.toString
        , expect = Http.expectJson GotReceipeList receipeListDecoder
        }


receipeListDecoder : JD.Decoder (List ReceipePreview)
receipeListDecoder =
    JD.list
        (JD.map3 ReceipePreview
            (JD.field "id" JD.int)
            (JD.field "title" JD.string)
            (JD.field "image_ids" (JD.list JD.int))
        )


getReceipe : (Result Http.Error Receipe.Receipe -> Msg) -> Int -> Cmd Msg
getReceipe msg id =
    Http.get
        { url = Api.Receipe id |> Api.toString
        , expect = Http.expectJson msg Receipe.decoder
        }


getNewReceipe : Cmd Msg
getNewReceipe =
    Http.post
        { url = UB.absolute [ "receipe", "create" ] []
        , body = Http.emptyBody
        , expect = Http.expectJson GotNewReceipe Receipe.decoder
        }
