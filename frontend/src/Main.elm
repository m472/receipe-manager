module Main exposing (..)


import Browser
import Html exposing (Html, button, div, text, p, h1, br, li, ul, b, img, main_, a, input, select, option)
import Html.Attributes exposing (src, width, height, href, value, type_, name)
import Html.Events exposing (onClick)
import Dict exposing (Dict)
import Http
import Json.Decode exposing (Decoder, map2, map3, map4, map5, map6, field, int, string, list, float, nullable, dict)


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
    , units: Dict String Unit
    }

type alias Unit =
    { id: String
    , symbol: String
    , si_conversion_factor: Float
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
    { amount: Maybe Float
    , unit: String
    , name: String
    , comment: Maybe String
    }

-- UPDATE


type Msg
  = GotReceipe (Result Http.Error Receipe)
  | EditReceipe Receipe
  | AddIngredientGroup Receipe
  | AddIngredient Receipe IngredientGroup
  | RemoveIngredientGroup Receipe IngredientGroup
  | RemoveIngredient Receipe IngredientGroup Ingredient
  | Save Receipe
  | CancelEdit


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
        (Edit {receipe | ingredients = (receipe.ingredients ++ [IngredientGroup "" []])}, Cmd.none)

    RemoveIngredientGroup receipe ingredientGroupToRemove ->
        (   Edit { receipe | ingredients = List.filter (\group -> group /= ingredientGroupToRemove) receipe.ingredients }
        ,   Cmd.none
        )

    AddIngredient receipe ingredientGroup ->
        let addToGroup group =
                if group == ingredientGroup then
                    { group | ingredients = group.ingredients ++ [Ingredient Nothing "" "" Nothing]}
                else
                    group
        in
        (   Edit { receipe | ingredients = (List.map addToGroup receipe.ingredients) }
        ,   Cmd.none
        )

    RemoveIngredient receipe ingredientGroup ingredientToRemove ->
        let
            updatedIngredientGroups =
                List.map
                (\ig -> if ig == ingredientGroup
                    then
                        { ingredientGroup | ingredients = (List.filter (\ingredient -> ingredient /= ingredientToRemove) ingredientGroup.ingredients) }
                    else
                        ig
                )
                receipe.ingredients
        in
        (   Edit { receipe | ingredients = updatedIngredientGroups }
        ,   Cmd.none
        )

    Save receipe ->
        (   Display receipe
        ,   Cmd.none
        )

    CancelEdit ->
        (   Loading
        ,   getReceipe
        )


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
            ,   div [] (List.map (viewIngredientGroup receipe.units) receipe.ingredients)
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
            ,   div [] (List.map (editIngredientGroup receipe) receipe.ingredients)
            ,   button [onClick (AddIngredientGroup receipe)] [text "Zutatengruppe hinzufügen"]
            ,   button [onClick (Save receipe)] [text "Speichern"]
            ,   button [onClick CancelEdit] [text "Abbrechen"]
            ]

viewImages : Int -> List Int -> Html Msg
viewImages receipe_id image_ids =
    div []
    (List.map (\img_id -> img [src ("/receipe/image?receipe_id=" ++ String.fromInt receipe_id ++ "&image_id=" ++ String.fromInt img_id), width 500] []) image_ids)

viewIngredientGroup : (Dict String Unit) -> IngredientGroup -> Html Msg
viewIngredientGroup units ingredientGroup =
    div []
    [    b [] [text ingredientGroup.name]
    ,    ul [] ( List.map (viewIngredient units) ingredientGroup.ingredients )
    ]

editIngredientGroup : Receipe -> IngredientGroup -> Html Msg
editIngredientGroup receipe ingredientGroup =
    div []
    [    b [] [text "Zutatengruppe: ", input [value ingredientGroup.name] []]
    ,    button [onClick (RemoveIngredientGroup receipe ingredientGroup)] [text "entfernen"]
    ,    ul [] ( List.map (editIngredient receipe ingredientGroup) ingredientGroup.ingredients )
    ,    button [onClick (AddIngredient receipe ingredientGroup)] [text "Zutat hinzufügen"]
    ]

viewIngredient : (Dict String Unit) -> Ingredient -> Html Msg
viewIngredient units ingredient =
    li []
    [   text (viewMaybeFloat ingredient.amount)
    ,   text (viewUnit ingredient.unit units)
    ,   text " "
    ,   text ingredient.name
    ]

viewMaybeFloat : Maybe Float -> String
viewMaybeFloat value =
    case value of
        Just floatValue ->
            String.fromFloat floatValue
        Nothing ->
            ""

viewUnit : String -> Dict String Unit -> String
viewUnit ingredient_unit_id units =
    case (Dict.get ingredient_unit_id units) of
        Just unit ->
            unit.symbol
        Nothing ->
            "??"

editIngredient : Receipe -> IngredientGroup -> Ingredient -> Html Msg
editIngredient receipe ingredientGroup ingredient =
    li []
    [   input [type_ "number", value (viewMaybeFloat ingredient.amount)] []
    ,   editUnit ingredient.unit receipe.units
    ,   text " "
    ,   input [type_ "text", value ingredient.name] []
    ,   button [onClick (RemoveIngredient receipe ingredientGroup ingredient)] [text "-"]
    ]

editUnit : String -> Dict String Unit -> Html Msg
editUnit ingredient_unit_id units =
    let
        selected = Dict.get ingredient_unit_id units
        sortedUnits = (case selected of
            Just unit ->
                unit :: (Dict.remove ingredient_unit_id units |> Dict.values)
            Nothing ->
                units |> Dict.values)
    in
        select [] (List.map (\unit -> option [value unit.id] [text unit.symbol]) sortedUnits)


-- HTTP

getReceipe : Cmd Msg
getReceipe =
    Http.get
    { url = "/receipe/json?id=0"
    , expect = Http.expectJson GotReceipe receipeDecoder
    }

receipeDecoder : Decoder Receipe
receipeDecoder =
    map6 Receipe
        (field "id" int)
        (field "title" string)
        (field "image_ids" (list int))
        (field "servings" servingsDecoder)
        (field "ingredients" (list ingredientGroupDecoder))
        (field "units" (dict unitDecoder))

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
        (field "amount" (nullable float))
        (field "unit" string)
        (field "name" string)
        (field "comment" (nullable string))

unitDecoder : Decoder Unit
unitDecoder =
    map3 Unit
        (field "id" string)
        (field "symbol" string)
        (field "si_conversion_factor" float)
