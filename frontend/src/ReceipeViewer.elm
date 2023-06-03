module ReceipeViewer exposing (..)

import Dict exposing (Dict)
import Helpers
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Receipe exposing (..)
import Route
import Url.Builder



-- MODEL


type Msg
    = Delete
    | Edit
    | ServingsChanged String
    | Deleted (Result Http.Error String)
    | NextImage
    | PrevImage


type alias Model =
    { receipe : Receipe.Receipe
    , servings : Int
    , activeImage : Int
    , errorMsg : String
    }



-- INIT


modelFromReceipe : Receipe -> String -> Model
modelFromReceipe receipe msg =
    Model receipe receipe.servings.amount 0 msg



-- VIEW


view : Model -> Html Msg
view model =
    let
        receipe =
            model.receipe

        scaling_factor =
            toFloat model.servings / toFloat receipe.servings.amount
    in
    div []
        [ a [ href "/" ] [ text "Alle Rezepte" ]
        , h1 [] [ text receipe.title ]
        , viewImages receipe model.activeImage
        , p []
            [ b []
                [ text "Zutaten für "
                , input
                    [ type_ "number"
                    , value (String.fromInt model.servings)
                    , onInput ServingsChanged
                    ]
                    []
                , text (" " ++ receipe.servings.unit ++ ":")
                ]
            ]
        , div [] (List.map (viewIngredientGroup scaling_factor receipe.units) receipe.ingredients)
        , button [ onClick Edit ] [ text "bearbeiten" ]
        , button [ onClick Delete ] [ text "löschen" ]
        ]


viewIngredientGroup : Float -> Dict String Unit -> IngredientGroup -> Html a
viewIngredientGroup factor units ingredientGroup =
    div []
        [ b [] [ text ingredientGroup.name ]
        , ul [] (List.map (viewIngredient factor units) ingredientGroup.ingredients)
        ]


viewIngredient : Float -> Dict String Unit -> Ingredient -> Html a
viewIngredient factor units ingredient =
    li []
        [ text (Helpers.viewMaybeFloat factor ingredient.amount)
        , text (viewUnit ingredient.unit units)
        , text " "
        , text ingredient.name
        , text " "
        , text (Maybe.withDefault "" ingredient.comment)
        ]


viewUnit : String -> Dict String Unit -> String
viewUnit ingredient_unit_id units =
    case Dict.get ingredient_unit_id units of
        Just unit ->
            unit.symbol

        Nothing ->
            "??"


viewImages : Receipe -> Int -> Html a
viewImages receipe active_img_nr =
    div []
        (List.indexedMap
            (\i img_id ->
                img
                    [ src
                        (Url.Builder.absolute [ "receipe", "image" ]
                            [ Url.Builder.int "receipe_id" receipe.id
                            , Url.Builder.int "image_id" img_id
                            ]
                        )
                    , width 500
                    , class
                        (if i == active_img_nr then
                            "active_img"

                         else
                            "inactive_img"
                        )
                    ]
                    []
            )
            receipe.image_ids
        )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Edit ->
            ( model, Route.load (Route.EditReceipe model.receipe.id) )

        ServingsChanged servingsStr ->
            case String.toInt servingsStr of
                Just value ->
                    ( { model | servings = value }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        Delete ->
            ( model, deleteReceipe model.receipe.id )

        Deleted result ->
            case result of
                Ok _ ->
                    ( model, Route.load Route.Overview )

                Err _ ->
                    ( model, Route.load Route.Overview )

        NextImage ->
            ( { model
                | activeImage =
                    modBy (model.activeImage + 1) (List.length model.receipe.image_ids)
              }
            , Cmd.none
            )

        PrevImage ->
            let
                i =
                    if model.activeImage > 1 then
                        model.activeImage - 1

                    else
                        List.length model.receipe.image_ids - 1
            in
            ( { model | activeImage = i }, Cmd.none )



-- HTTP


deleteReceipe : Int -> Cmd Msg
deleteReceipe id =
    Http.post
        { url = "/receipe/delete?id=" ++ String.fromInt id
        , body = Http.emptyBody
        , expect = Http.expectString Deleted
        }
