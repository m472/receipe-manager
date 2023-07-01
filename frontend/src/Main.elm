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
import List
import Platform.Cmd as Cmd
import Receipe
import ReceipeEditor
import ReceipeViewer
import Route exposing (Route(..))
import Set
import Url
import Url.Builder as UB
import Url.Parser



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
    | ViewReceipeList (Maybe String) (List ReceipePreview) String
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
    , tags : List String
    }



-- UPDATE


type Msg
    = GotReceipe (Result Http.Error Receipe.Receipe)
    | GotReceipeForEdit (Result Http.Error Receipe.Receipe)
    | GotNewReceipe (Result Http.Error Receipe.Receipe)
    | GotReceipeList (Maybe String) (Result Http.Error (List ReceipePreview))
    | RoutedEditMsg ReceipeEditor.Model ReceipeEditor.Msg
    | RoutedImportMsg Importer.Model Importer.Msg
    | RoutedReceipeMsg ReceipeViewer.Model ReceipeViewer.Msg
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | SearchChanged String
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
                    ( { model
                        | content =
                            ReceipeEditor
                                { currentImage = 0, receipe = ReceipeEditor.toEditable receipe, errorMessage = "" }
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model
                        | content = Failure "Rezept zum Editieren laden fehlgeschlagen"
                      }
                    , Cmd.none
                    )

        GotNewReceipe result ->
            case result of
                Ok receipe ->
                    ( { model | content = ReceipeViewer (ReceipeViewer.modelFromReceipe receipe "") }, Cmd.none )

                Err _ ->
                    ( { model | content = Failure "Rezept konnte nicht geladen werden" }, Cmd.none )

        GotReceipeList maybeTag result ->
            case ( maybeTag, result ) of
                ( tag, Ok receipes ) ->
                    ( { model | content = ViewReceipeList tag receipes "" }, Cmd.none )

                ( _, Err _ ) ->
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

        SearchChanged query ->
            case model.content of
                ViewReceipeList tags receipes _ ->
                    ( { model | content = ViewReceipeList tags receipes query }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        CreateReceipe ->
            ( { model | content = Loading "neues Rezept" }, getNewReceipe )

        OpenReceipeImporter ->
            ( { model | content = Loading "Rezept Importer" }, Route.load Route.ImportReceipe )


onUrlChange : Url.Url -> ( ModelContent, Cmd Msg )
onUrlChange url =
    case Url.Parser.parse Route.parse url of
        Just (Route.ViewReceipe id) ->
            ( Loading ("Receipe " ++ String.fromInt id), getReceipe GotReceipe id )

        Just (Route.EditReceipe id) ->
            ( Loading ("ReceipeEditor" ++ String.fromInt id), getReceipe GotReceipeForEdit id )

        Just (Route.Overview maybeTag) ->
            ( Loading "Overview", getReceipeList (GotReceipeList maybeTag) )

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

                ViewReceipeList tag receipeList searchQuery ->
                    viewReceipeOverview tag searchQuery receipeList

                ReceipeImporter importerModel ->
                    Html.map (RoutedImportMsg importerModel)
                        (Importer.view importerModel)
            ]
        ]


viewReceipeOverview : Maybe String -> String -> List ReceipePreview -> Html Msg
viewReceipeOverview tag searchQuery receipeList =
    div []
        [ viewNavBar receipeList
        , h1 [] [ text "Rezepte-Übersicht" ]
        , div []
            [ button [ onClick CreateReceipe, class "rounded-button" ] [ text "neues Rezept" ]
            , button [ onClick OpenReceipeImporter, class "rounded-button" ] [ text "Rezept importieren" ]
            ]
        , input [ type_ "search", placeholder "Suche...", onInput SearchChanged, class "searchbox" ] []
        , viewReceipes searchQuery (searchReceipes searchQuery receipeList |> filterByTag tag)
        ]


viewReceipes : String -> List ReceipePreview -> Html a
viewReceipes searchQuery receipeList =
    let
        getTitle =
            \receipe ->
                case receipe.title of
                    "" ->
                        "Untitled"

                    title ->
                        title
    in
    ul [ class "receipe-list" ]
        (List.map
            (\receipe ->
                let
                    receipeLink =
                        href ("/receipe/" ++ String.fromInt receipe.id)
                in
                li []
                    [ div [ class "receipe-in-overview" ]
                        [ a [ receipeLink ]
                            [ img
                                [ src
                                    (UB.absolute [ "receipe", "image" ]
                                        [ UB.int "receipe_id" receipe.id
                                        , UB.int "image_id" (List.head receipe.image_ids |> Maybe.withDefault 0)
                                        ]
                                    )
                                , class "receipe-thumb"
                                ]
                                []
                            ]
                        , div []
                            (div []
                                [ a [ receipeLink, class "receipe-overview-title" ]
                                    (highlightSearchResult searchQuery (getTitle receipe))
                                ]
                                :: List.map
                                    (\t ->
                                        a
                                            [ href (UB.absolute [] [ UB.string "tag" t ])
                                            , class "tag-button"
                                            , class "rounded-button"
                                            ]
                                            (highlightSearchResult searchQuery t)
                                    )
                                    receipe.tags
                            )
                        ]
                    ]
            )
            receipeList
        )


highlightSearchResult : String -> String -> List (Html a)
highlightSearchResult query txt =
    let
        indices =
            String.indices (String.toLower query) (String.toLower txt)
    in
    case query of
        "" ->
            [ text txt ]

        _ ->
            highlightSearchResult_ (String.length query) indices txt


highlightSearchResult_ : Int -> List Int -> String -> List (Html a)
highlightSearchResult_ length startingPositions txt =
    case startingPositions of
        [] ->
            [ text txt ]

        x :: xs ->
            text (String.left x txt)
                :: mark [] [ String.slice x (x + length) txt |> text ]
                :: highlightSearchResult_ length xs (String.dropLeft (length + x) txt)


searchReceipes : String -> List ReceipePreview -> List ReceipePreview
searchReceipes searchQuery receipeList =
    let
        getFields =
            \r -> r.title :: r.tags

        matches =
            String.toLower >> String.contains (String.toLower searchQuery)
    in
    List.filter (\r -> List.any matches (getFields r)) receipeList


filterByTag : Maybe String -> List ReceipePreview -> List ReceipePreview
filterByTag maybeTag receipeList =
    case maybeTag of
        Just tag ->
            List.filter (\r -> List.member tag r.tags) receipeList

        Nothing ->
            receipeList


viewNavBar : List ReceipePreview -> Html a
viewNavBar receipeList =
    let
        tags =
            List.concatMap .tags receipeList |> Set.fromList |> Set.toList
    in
    nav []
        (a [ href "/", class "tag-button", class "rounded-button" ] [ text "Alle Rezepte" ]
            :: List.map (\t -> a [ href (UB.absolute [] [ UB.string "tag" t ]), class "tag-button", class "rounded-button" ] [ text t ]) tags
        )



-- HTTP


getReceipeList : (Result Http.Error (List ReceipePreview) -> Msg) -> Cmd Msg
getReceipeList msg =
    Api.get
        { endpoint = Api.ReceipeList
        , expect = Http.expectJson msg receipeListDecoder
        }


receipeListDecoder : JD.Decoder (List ReceipePreview)
receipeListDecoder =
    JD.list
        (JD.map4 ReceipePreview
            (JD.field "id" JD.int)
            (JD.field "title" JD.string)
            (JD.field "image_ids" (JD.list JD.int))
            (JD.field "tags" (JD.list JD.string))
        )


getReceipe : (Result Http.Error Receipe.Receipe -> Msg) -> Int -> Cmd Msg
getReceipe msg id =
    Api.get
        { endpoint = Api.Receipe id
        , expect = Http.expectJson msg Receipe.decoder
        }


getNewReceipe : Cmd Msg
getNewReceipe =
    Api.post
        { endpoint = Api.CreateReceipe
        , body = Http.emptyBody
        , expect = Http.expectJson GotNewReceipe Receipe.decoder
        }
