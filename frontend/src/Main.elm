module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Http
import Json.Decode as JD exposing (Error(..))
import List
import Receipe
import ReceipeEditor
import ReceipeViewer
import Route exposing (Route(..))
import Set
import StyledElements exposing (..)
import Url
import Url.Builder
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
    = Failure
    | Loading String
    | ReceipeViewer ReceipeViewer.Model
    | ReceipeEditor ReceipeEditor.Model
    | ViewReceipeList (Maybe String) (List ReceipePreview) String


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
    | RoutedReceipeMsg ReceipeViewer.Model ReceipeViewer.Msg
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | SearchChanged String
    | CreateReceipe


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
                    ( { model | content = Failure }, Cmd.none )

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
                    ( { model | content = Failure }, Cmd.none )

        GotNewReceipe result ->
            case result of
                Ok receipe ->
                    ( { model | content = ReceipeViewer (ReceipeViewer.modelFromReceipe receipe "") }, Cmd.none )

                Err _ ->
                    ( { model | content = Failure }, Cmd.none )

        GotReceipeList maybeTag result ->
            case ( maybeTag, result ) of
                ( tag, Ok receipes ) ->
                    ( { model | content = ViewReceipeList tag receipes "" }, Cmd.none )

                ( _, Err _ ) ->
                    ( { model | content = Failure }, Cmd.none )

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
            ( { model | content = Loading "new Receipe" }, getNewReceipe )


onUrlChange : Url.Url -> ( ModelContent, Cmd Msg )
onUrlChange url =
    case Url.Parser.parse Route.parse url of
        Just (Route.ViewReceipe id) ->
            ( Loading ("Receipe " ++ String.fromInt id), getReceipe GotReceipe id )

        Just (Route.EditReceipe id) ->
            ( Loading ("ReceipeEditor" ++ String.fromInt id), getReceipe GotReceipeForEdit id )

        Just (Route.Overview maybeTag) ->
            ( Loading "Overview", getReceipeList (GotReceipeList maybeTag) )

        Nothing ->
            ( Failure, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    Browser.Document
        "Receipe Manager"
        (List.map toUnstyled
            [ main_ []
                [ case model.content of
                    Failure ->
                        text "that went wrong..."

                    Loading msg ->
                        text ("loading " ++ msg ++ " ...")

                    ReceipeViewer receipeViewerModel ->
                        Html.Styled.map (RoutedReceipeMsg receipeViewerModel)
                            (ReceipeViewer.view receipeViewerModel)

                    ReceipeEditor receipeEditorModel ->
                        Html.Styled.map (RoutedEditMsg receipeEditorModel)
                            (ReceipeEditor.view receipeEditorModel)

                    ViewReceipeList tag receipeList searchQuery ->
                        viewReceipeOverview tag searchQuery receipeList
                ]
            ]
        )


viewReceipeOverview : Maybe String -> String -> List ReceipePreview -> Html Msg
viewReceipeOverview tag searchQuery receipeList =
    div []
        [ viewNavBar receipeList
        , h1 [] [ text "Rezepte-Übersicht" ]
        , button [ onClick CreateReceipe ] [ text "neues Rezept" ]
        , input [ type_ "search", placeholder "Suche...", onInput SearchChanged ] []
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
    ul []
        (List.map
            (\receipe ->
                let
                    receipeLink =
                        href ("/receipe/" ++ String.fromInt receipe.id)
                in
                li
                    [ css [ displayFlex ]
                    ]
                    [ a [ receipeLink ]
                        [ img
                            [ src
                                (Url.Builder.absolute [ "receipe", "image" ]
                                    [ Url.Builder.int "receipe_id" receipe.id
                                    , Url.Builder.int "image_id" (List.head receipe.image_ids |> Maybe.withDefault 0)
                                    ]
                                )
                            , css
                                [ Css.width (ex 12)
                                ]
                            ]
                            []
                        ]
                    , div [ css [ margin2 (pt 0) (pt 10) ] ]
                        (div
                            [ css
                                [ fontFamilies [ "Helvetica", "Arial" ]
                                , margin (pt 2)
                                , fontSize (pt 16)
                                ]
                            ]
                            [ a [ receipeLink, css [ textDecoration none, color (hex "000000") ] ]
                                (highlightSearchResult searchQuery (getTitle receipe))
                            ]
                            :: List.map
                                (\t ->
                                    tagButton
                                        [ href (Url.Builder.absolute [] [ Url.Builder.string "tag" t ]) ]
                                        (highlightSearchResult searchQuery t)
                                )
                                receipe.tags
                        )
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
        (tagButton [ href "/" ] [ text "Alle Rezepte" ]
            :: List.map (\t -> tagButton [ href ("/?tag=" ++ t) ] [ text t ]) tags
        )



-- HTTP


getReceipeList : (Result Http.Error (List ReceipePreview) -> Msg) -> Cmd Msg
getReceipeList msg =
    Http.get
        { url = "/receipe/list"
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
    Http.get
        { url = "/receipe?id=" ++ String.fromInt id
        , expect = Http.expectJson msg Receipe.decoder
        }


getNewReceipe : Cmd Msg
getNewReceipe =
    Http.post
        { url = "/receipe/create"
        , body = Http.emptyBody
        , expect = Http.expectJson GotNewReceipe Receipe.decoder
        }
