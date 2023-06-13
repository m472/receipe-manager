module ReceipeViewer exposing (..)

import Api
import Dict exposing (Dict)
import Helpers
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Http
import Receipe exposing (..)
import ReceipeImageViewer exposing (viewImages)
import Route
import StyledElements exposing (..)



-- MODEL


type Msg
    = Delete
    | Edit
    | ServingsChanged String
    | Deleted (Result Http.Error String)
    | ImageViewerMsg ReceipeImageViewer.Msg


type alias Model =
    { receipe : Receipe.Receipe
    , servings : Int
    , currentImage : Int
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
        [ a [ href (Route.Overview Nothing |> Route.toString) ] [ text "Alle Rezepte" ]
        , h1 [] [ text receipe.title ]
        , div [] (List.map (\t -> tagButton [] [ text t ]) receipe.tags)
        , Html.Styled.map ImageViewerMsg (viewImages model.receipe.id model.receipe.image_ids model.currentImage)
        , h2 [] [ text "Zutaten" ]
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
        , h2 [] [ text "Zubereitung" ]
        , div [] (List.map viewInstructionGroup receipe.instructions)
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


viewInstructionGroup : InstructionGroup -> Html a
viewInstructionGroup group =
    div []
        [ b [] [ text group.name ]
        , ul [] (List.map (\step -> li [] [ text step ]) group.steps)
        ]



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
            ( model, Api.deleteReceipe Deleted model.receipe.id )

        Deleted result ->
            case result of
                Ok _ ->
                    ( model, Route.load (Route.Overview Nothing) )

                Err _ ->
                    ( model, Route.load (Route.Overview Nothing) )

        ImageViewerMsg childMsg ->
            ( { model
                | currentImage =
                    ReceipeImageViewer.update childMsg
                        model.receipe.image_ids
                        model.currentImage
              }
            , Cmd.none
            )
