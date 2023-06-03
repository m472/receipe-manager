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


type alias Model =
    { receipe : Receipe.Receipe
    , servings : Int
    , errorMsg : String
    }



-- INIT


modelFromReceipe : Receipe -> String -> Model
modelFromReceipe receipe msg =
    Model receipe receipe.servings.amount msg



-- VIEW


view : Model -> Html Msg
view scaled_receipe =
    let
        receipe =
            scaled_receipe.receipe

        scaling_factor =
            toFloat scaled_receipe.servings / toFloat receipe.servings.amount
    in
    div []
        [ a [ href "/" ] [ text "Alle Rezepte" ]
        , h1 [] [ text receipe.title ]
        , viewImages receipe
        , p []
            [ b []
                [ text "Zutaten für "
                , input
                    [ type_ "number"
                    , value (String.fromInt scaled_receipe.servings)
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


viewImages : Receipe -> Html a
viewImages receipe =
    div []
        (List.map
            (\img_id ->
                img
                    [ src
                        (Url.Builder.absolute [ "receipe", "image" ]
                            [ Url.Builder.int "receipe_id" receipe.id
                            , Url.Builder.int "image_id" img_id
                            ]
                        )
                    , width 500
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



-- HTTP


deleteReceipe : Int -> Cmd Msg
deleteReceipe id =
    Http.post
        { url = "/receipe/delete?id=" ++ String.fromInt id
        , body = Http.emptyBody
        , expect = Http.expectString Deleted
        }
