module Receipe exposing (Ingredient, IngredientGroup, Receipe, Unit, decoder, encoder)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as JD
import Json.Encode as JE
import Route


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
