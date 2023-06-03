module Receipe exposing (Ingredient, IngredientGroup, Msg, Receipe, Model, Unit, decoder, encoder, modelFromReceipe, update, view, viewImages)

import Browser.Navigation as Nav

import Dict exposing (Dict)
import Helpers
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as JD
import Json.Encode as JE
import Route



-- MODEL


type Msg
    = Delete
    | Edit
    | ServingsChanged String
    | Deleted (Result Http.Error String)


type alias Model =
    { receipe : Receipe
    , servings : Int
    , errorMsg : String
    }


type alias Receipe =
    { id : Int
    , title : String
    , image_ids : List Int
    , servings : Servings
    , ingredients : List IngredientGroup
    , units : Dict String Unit
    }


type alias Servings =
    { amount : Int
    , unit : String
    }


type alias IngredientGroup =
    { name : String
    , ingredients : List Ingredient
    }


type alias Ingredient =
    { amount : Maybe Float
    , unit : String
    , name : String
    , comment : Maybe String
    }


type alias Unit =
    { id : String
    , symbol : String
    , si_conversion_factor : Float
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
        [ a [href "/"] [text "Alle Rezepte"]
        , h1 [] [ text receipe.title ]
        , viewImages receipe.id receipe.image_ids
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


viewImages : Int -> List Int -> Html a
viewImages receipe_id image_ids =
    div []
        (List.map
            (\img_id ->
                img
                    [ src
                        ("/receipe/image?receipe_id="
                            ++ String.fromInt receipe_id
                            ++ "&image_id="
                            ++ String.fromInt img_id
                        )
                    , width 500
                    ]
                    []
            )
            image_ids
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
            Debug.log "delete" ( model, deleteReceipe model.receipe.id )

        Deleted result ->
            case result of
                Ok _ ->
                    ( model, Route.load Route.Overview )
                Err _ ->
                    ( model, Route.load Route.Overview )




-- ENCODERS


encoder : Receipe -> JE.Value
encoder receipe =
    JE.object
        [ ( "title", JE.string receipe.title )
        , ( "id", JE.int receipe.id )
        , ( "ingredients", JE.list ingredientGroupEncoder receipe.ingredients )
        , ( "image_ids", JE.list JE.int receipe.image_ids )
        , ( "servings", servingsEncoder receipe.servings )
        ]


ingredientGroupEncoder : IngredientGroup -> JE.Value
ingredientGroupEncoder group =
    JE.object
        [ ( "name", JE.string group.name )
        , ( "ingredients", JE.list ingredientEncoder group.ingredients )
        ]


ingredientEncoder : Ingredient -> JE.Value
ingredientEncoder ingredient =
    JE.object
        [ ( "amount", maybeEncoder JE.float ingredient.amount )
        , ( "unit", JE.string ingredient.unit )
        , ( "name", JE.string ingredient.name )
        , ( "comment", maybeEncoder JE.string ingredient.comment )
        ]


servingsEncoder : Servings -> JE.Value
servingsEncoder servings =
    JE.object
        [ ( "amount", JE.int servings.amount )
        , ( "unit", JE.string servings.unit )
        ]


maybeEncoder : (a -> JE.Value) -> Maybe a -> JE.Value
maybeEncoder f value =
    case value of
        Just val ->
            f val

        Nothing ->
            JE.null



-- DECODERS


decoder : JD.Decoder Receipe
decoder =
    JD.map6 Receipe
        (JD.field "id" JD.int)
        (JD.field "title" JD.string)
        (JD.field "image_ids" (JD.list JD.int))
        (JD.field "servings" servingsDecoder)
        (JD.field "ingredients" (JD.list ingredientGroupDecoder))
        (JD.field "units" (JD.dict unitDecoder))


servingsDecoder : JD.Decoder Servings
servingsDecoder =
    JD.map2 Servings
        (JD.field "amount" JD.int)
        (JD.field "unit" JD.string)


ingredientGroupDecoder : JD.Decoder IngredientGroup
ingredientGroupDecoder =
    JD.map2 IngredientGroup
        (JD.field "name" JD.string)
        (JD.field "ingredients" (JD.list ingredientDecoder))


ingredientDecoder : JD.Decoder Ingredient
ingredientDecoder =
    JD.map4 Ingredient
        (JD.field "amount" (JD.nullable JD.float))
        (JD.field "unit" JD.string)
        (JD.field "name" JD.string)
        (JD.field "comment" (JD.nullable JD.string))


unitDecoder : JD.Decoder Unit
unitDecoder =
    JD.map3 Unit
        (JD.field "id" JD.string)
        (JD.field "symbol" JD.string)
        (JD.field "si_conversion_factor" JD.float)



-- HTTP


deleteReceipe : Int -> Cmd Msg
deleteReceipe id =
    Http.post
        { url = "/receipe/delete?id=" ++ String.fromInt id
        , body = Http.emptyBody
        , expect = Http.expectString Deleted
        }
