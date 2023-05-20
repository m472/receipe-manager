module Main exposing (..)


import Browser
import Html exposing (Html, button, div, text, p, h1, br, li, ul, b, img, main_, a, input)
import Html.Attributes exposing (src, width, height, href, value, type_)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, map2, map3, map4, map5, field, int, string, list, float, nullable)


-- MAIN


main =
  Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }

-- MODEL

type Model
    = Failure
    | Loading
    | Display Receipe
    | Edit Receipe


init : () -> (Model, Cmd Msg)
init _ =
  (Loading, getReceipe)

type alias Receipe =
    { id: Int
    , title: String
    , image_ids: List Int
    , servings: Servings
    , ingredients: List IngredientGroup
    }

type alias Servings =
    { amount: Int
    , unit: String
    }

type alias IngredientGroup =
    { name: String
    , ingredients: List Ingredient
    }

type alias Ingredient =
    { amount: Float
    , unit: String
    , name: String
    , comment: Maybe String
    }

-- UPDATE


type Msg
  = GotReceipe (Result Http.Error Receipe)
  | EditReceipe Receipe
  | AddIngredientGroup Receipe


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotReceipe result ->
        case result of
            Ok receipe ->
              (Display receipe, Cmd.none)

            Err _ ->
              (Failure, Cmd.none)

    EditReceipe receipe ->
        (Edit receipe, Cmd.none)

    AddIngredientGroup receipe ->
        (Edit (Receipe receipe.id receipe.title receipe.image_ids receipe.servings (receipe.ingredients ++ [IngredientGroup "" []])), Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- VIEW

view : Model -> Html Msg
view model =
  main_ [] [ viewReceipe model ]

viewReceipe : Model -> Html Msg
viewReceipe model =
    case model of
        Failure -> text "that went wrong..."
        Loading -> text "loading..."
        Display receipe ->
            div []
            [   h1 [] [ text receipe.title ]
            ,   viewImages receipe.id receipe.image_ids
            ,   p [] [b [] [text ("Zutaten für " ++ String.fromInt receipe.servings.amount ++ " " ++ receipe.servings.unit ++ ":")]]
            ,   div [] (List.map viewIngredientGroup receipe.ingredients)
            ,   button [onClick (EditReceipe receipe)] [text "edit"]
            ]
        Edit receipe ->
            div []
            [   h1 [] [ text "Titel: ", input [value receipe.title] [] ]
            ,   viewImages receipe.id receipe.image_ids
            ,   p [] [b []
                [   text "Zutaten für "
                ,   input [type_ "number", value (String.fromInt receipe.servings.amount)] []
                ,   text " "
                ,   input [value receipe.servings.unit] []
                ,   text ":"]]
            ,   div [] (List.map editIngredientGroup receipe.ingredients)
            ,   button [onClick (AddIngredientGroup receipe)] [text "Zutatengruppe hinzufügen"]
            ,   button [] [text "Speichern"]
            ,   button [] [text "Abbrechen"]
            ]

viewImages : Int -> List Int -> Html Msg
viewImages receipe_id image_ids =
    div []
    (List.map (\img_id -> img [src ("/receipe/image?receipe_id=" ++ String.fromInt receipe_id ++ "&image_id=" ++ String.fromInt img_id), width 500] []) image_ids)

viewIngredientGroup : IngredientGroup -> Html Msg
viewIngredientGroup ingredientGroup =
    div []
    [    b [] [text ingredientGroup.name]
    ,    ul [] ( List.map viewIngredient ingredientGroup.ingredients )
    ]

editIngredientGroup : IngredientGroup -> Html Msg
editIngredientGroup ingredientGroup =
    div []
    [    b [] [text "Zutatengruppe: ", input [value ingredientGroup.name] []]
    ,    button [] [text "entfernen"]
    ,    ul [] ( List.map editIngredient ingredientGroup.ingredients )
    ,    button [] [text "Zutat hinzufügen"]
    ]

viewIngredient : Ingredient -> Html Msg
viewIngredient ingredient =
    li []
    [   text (String.fromFloat ingredient.amount)
    ,   text ingredient.unit
    ,   text " "
    ,   text ingredient.name
    ]

editIngredient : Ingredient -> Html Msg
editIngredient ingredient =
    li []
    [   input [type_ "number", value (String.fromFloat ingredient.amount)] []
    ,   input [type_ "text", value ingredient.unit] []
    ,   text " "
    ,   input [type_ "text", value ingredient.name] []
    ,   button [ ] [text "-"]
    ]

-- HTTP

getReceipe : Cmd Msg
getReceipe =
    Http.get
    { url = "/receipe/json?id=0"
    , expect = Http.expectJson GotReceipe receipeDecoder
    }

receipeDecoder : Decoder Receipe
receipeDecoder =
    map5 Receipe
        (field "id" int)
        (field "title" string)
        (field "image_ids" (list int))
        (field "servings" servingsDecoder)
        (field "ingredients" (list ingredientGroupDecoder))

servingsDecoder : Decoder Servings
servingsDecoder =
    map2 Servings
        (field "amount" int)
        (field "unit" string)

ingredientGroupDecoder : Decoder IngredientGroup
ingredientGroupDecoder =
    map2 IngredientGroup
        (field "name" string)
        (field "ingredients" (list ingredientDecoder))

ingredientDecoder : Decoder Ingredient
ingredientDecoder =
    map4 Ingredient
        (field "amount" float)
        (field "unit" string)
        (field "name" string)
        (field "comment" (nullable string))
