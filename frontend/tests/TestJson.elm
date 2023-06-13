module TestJson exposing (..)

import Dict exposing (Dict)
import Expect
import Helpers exposing (..)
import Json.Decode as JD
import Json.Encode as JE
import Receipe
import Test exposing (..)


testUnits : Dict String Receipe.Unit
testUnits =
    Dict.fromList
        [ ( "gr", { id = "gr", symbol = "g", si_conversion_factor = 1 } )
        , ( "", { id = "", symbol = "", si_conversion_factor = 1 } )
        ]


testIngredients : List Receipe.Ingredient
testIngredients =
    [ { amount = Nothing, unit = "", name = "Salz und Pfeffer", comment = Nothing }
    , { amount = Just 23.13, unit = "gr", name = "Weissmehl", comment = Just "optional" }
    ]


testIngredientGroups : List Receipe.IngredientGroup
testIngredientGroups =
    [ { name = ""
      , ingredients = []
      }
    ]


minimalReceipe : Receipe.Receipe
minimalReceipe =
    { id = 42
    , title = "Test title"
    , image_ids = []
    , ingredients = []
    , servings = { amount = 5, unit = "Portionen" }
    , tags = []
    , instructions = []
    , units = Dict.empty
    }


testReceipe : Receipe.Receipe
testReceipe =
    { minimalReceipe
        | image_ids = [ 3, 1, 2 ]
        , ingredients = testIngredientGroups
        , tags = ["oneword", "two words", "ALL_CAPS", "test & test"]
        , units = testUnits
    }


suite : Test
suite =
    describe "json module"
        [ test "round trip" <|
            \_ ->
                let
                    encoded =
                        JE.encode 0 (Receipe.encoder minimalReceipe)

                    result =
                        JD.decodeString Receipe.decoder encoded
                in
                case result of
                    Ok decoded ->
                        Expect.equal decoded minimalReceipe

                    Err msg ->
                        Expect.fail ("Error in javascript decode: " ++ Debug.toString msg)
        ]
