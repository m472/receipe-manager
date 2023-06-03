module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as JD
import Receipe exposing (ScaledReceipe)
import ReceipeEditor
import Url
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
    | ReceipeViewer ScaledReceipe String
    | ReceipeEditor Receipe.Receipe
    | ViewReceipeList (List ReceipePreview)


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
    | GotNewReceipe (Result Http.Error Receipe.Receipe)
    | GotReceipeList (Result Http.Error (List ReceipePreview))
    | EditReceipe
    | DeleteReceipe
    | CancelEdit
    | ReceipeServingsChanged String
    | RoutedEditMsg Receipe.Receipe ReceipeEditor.Msg
    | RoutedReceipeMsg ScaledReceipe Receipe.Msg
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | CreateReceipe


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotReceipe result ->
            case result of
                Ok receipe ->
                    ( { model | content = ReceipeViewer (Receipe.toScaled receipe) "" }, Cmd.none )

                Err _ ->
                    ( { model | content = Failure }, Cmd.none )

        GotNewReceipe result ->
            case result of
                Ok receipe ->
                    ( { model | content = ReceipeEditor receipe }, Cmd.none )

                Err _ ->
                    ( { model | content = Failure }, Cmd.none )

        GotReceipeList result ->
            case result of
                Ok receipes ->
                    ( { model | content = ViewReceipeList receipes }, Cmd.none )

                Err _ ->
                    ( { model | content = Failure }, Cmd.none )

        EditReceipe ->
            case model.content of
                ReceipeViewer scaled_receipe info ->
                    ( { model | content = ReceipeEditor scaled_receipe.receipe }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        CancelEdit ->
            case model.content of
                ReceipeEditor receipe ->
                    ( { model | content = Loading "ReceipeEditor" }
                    , getReceipe receipe.id
                    )

                _ ->
                    ( model, Cmd.none )

        ReceipeServingsChanged servingsStr ->
            let
                servingsFactor =
                    String.toInt servingsStr
            in
            case ( servingsFactor, model.content ) of
                ( Just value, ReceipeViewer scaled_receipe info ) ->
                    ( { model | content = ReceipeViewer { scaled_receipe | servings = value } info }
                    , Cmd.none
                    )

                ( _, _ ) ->
                    ( model, Cmd.none )

        RoutedEditMsg receipe childMsg ->
            let
                ( updatedContent, cmd ) =
                    ReceipeEditor.updateReceipe childMsg receipe
            in
            ( { model | content = ReceipeEditor updatedContent }, Cmd.map (RoutedEditMsg receipe) cmd )

        RoutedReceipeMsg scaled_receipe childMsg ->
            let
                ( updatedReceipe, cmd ) =
                    Receipe.update childMsg scaled_receipe
            in
            ( { model | content = ReceipeViewer updatedReceipe "" }
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

        CreateReceipe ->
            ( { model | content = Loading "new Receipe" }, getNewReceipe )

        DeleteReceipe ->
            Debug.todo "DeleteReceipe"


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg _ ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


onUrlChange : Url.Url -> ( ModelContent, Cmd Msg )
onUrlChange url =
    case Url.Parser.parse routeParser url of
        Just (ReceipeRoute id) ->
            ( Loading ("Receipe " ++ String.fromInt id), getReceipe id )

        Just (EditReceipeRoute id) ->
            ( Loading ("ReceipeEditor" ++ String.fromInt id), getReceipe id )

        Just OverviewRoute ->
            ( Loading "Overview", getReceipeList )

        Nothing ->
            ( Failure, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    Browser.Document
        "Receipe Manager"
        [ main_ []
            [ case model.content of
                Failure ->
                    text "that went wrong..."

                Loading msg ->
                    text ("loading " ++ msg ++ " ...")

                ReceipeViewer receipe _ ->
                    Html.map (RoutedReceipeMsg receipe) (Receipe.viewReceipe receipe)

                ReceipeEditor receipe ->
                    Html.map (RoutedEditMsg receipe) (ReceipeEditor.editReceipe receipe)

                ViewReceipeList receipeList ->
                    viewReceipeList receipeList
            ]
        ]


viewReceipeList : List ReceipePreview -> Html Msg
viewReceipeList receipeList =
    div []
        [ h1 [] [ text "Rezepte-Übersicht" ]
        , button [ onClick CreateReceipe ] [ text "neues Rezept" ]
        , ul []
            (List.map
                (\receipe ->
                    li []
                        [ a [ href ("/receipe/" ++ String.fromInt receipe.id) ]
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
        { url = "/receipe/list"
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


getReceipe : Int -> Cmd Msg
getReceipe id =
    Http.get
        { url = "/receipe?id=" ++ String.fromInt id
        , expect = Http.expectJson GotReceipe Receipe.receipeDecoder
        }


getNewReceipe : Cmd Msg
getNewReceipe =
    Http.post
        { url = "/receipe/create"
        , body = Http.emptyBody
        , expect = Http.expectJson GotNewReceipe Receipe.receipeDecoder
        }



-- URL PARSING


type Route
    = OverviewRoute
    | ReceipeRoute Int
    | EditReceipeRoute Int


routeParser : Url.Parser.Parser (Route -> a) a
routeParser =
    Url.Parser.oneOf
        [ Url.Parser.map ReceipeRoute (Url.Parser.s "receipe" </> Url.Parser.int)
        , Url.Parser.map EditReceipeRoute (Url.Parser.s "receipe" </> Url.Parser.s "edit" </> Url.Parser.int)
        , Url.Parser.map OverviewRoute Url.Parser.top
        ]
