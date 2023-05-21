module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode
    exposing
        ( Decoder
        , dict
        , field
        , float
        , int
        , list
        , map2
        , map3
        , map4
        , map5
        , map6
        , nullable
        , string
        )



-- MAIN


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type Mode
    = Display
    | Edit


type Model
    = Failure
    | Loading
    | ViewReceipe Receipe Mode


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getReceipe )


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



-- UPDATE


type Msg
    = GotReceipe (Result Http.Error Receipe)
    | EditReceipe
    | Save
    | CancelEdit
    | RoutedReceipeMsg ReceipeMsg


type ReceipeMsg
    = AddIngredientGroup
    | RemoveIngredientGroup Int
    | UpdateTitle String
    | UpdateServingsAmount String
    | UpdateServingsUnit String
    | RoutedIngredientGroupMsg Int IngredientGroupMsg


type IngredientMsg
    = UpdateName String
    | UpdateUnit String
    | UpdateComment String
    | UpdateAmount String


type IngredientGroupMsg
    = UpdateGroupName String
    | AddIngredient
    | RemoveIngredient Int
    | RoutedIngredientMsg Int Int IngredientMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotReceipe result ->
            case result of
                Ok receipe ->
                    ( ViewReceipe receipe Display, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )

        EditReceipe ->
            case model of
                ViewReceipe receipe _ ->
                    ( ViewReceipe receipe Edit, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Save ->
            case model of
                ViewReceipe receipe Edit ->
                    ( ViewReceipe receipe Display, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        CancelEdit ->
            ( Loading
            , getReceipe
            )

        RoutedReceipeMsg childMsg ->
            case model of
                ViewReceipe receipe Edit ->
                    ( ViewReceipe (updateReceipe childMsg receipe) Edit
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


updateReceipe : ReceipeMsg -> Receipe -> Receipe
updateReceipe msg model =
    case msg of
        AddIngredientGroup ->
            { model | ingredients = model.ingredients ++ [ IngredientGroup "" [] ] }

        UpdateTitle newName ->
            { model | title = newName }

        UpdateServingsUnit newUnit ->
            let
                servs =
                    model.servings
            in
            { model | servings = { servs | unit = newUnit } }

        UpdateServingsAmount amountStr ->
            let
                servs =
                    model.servings

                newAmount =
                    String.toInt amountStr
            in
            case newAmount of
                Just value ->
                    { model | servings = { servs | amount = value } }

                _ ->
                    model

        RemoveIngredientGroup index ->
            { model | ingredients = removeElementAt index model.ingredients }

        RoutedIngredientGroupMsg groupIndex childMsg ->
            let
                groups =
                    List.indexedMap
                        (\i ig ->
                            if i == groupIndex then
                                updateIngredientGroup childMsg ig

                            else
                                ig
                        )
                        model.ingredients
            in
            { model | ingredients = groups }


updateIngredient : IngredientMsg -> Ingredient -> Ingredient
updateIngredient msg model =
    case msg of
        UpdateName newName ->
            { model | name = newName }

        UpdateUnit newUnitId ->
            { model | unit = newUnitId }

        UpdateComment newComment ->
            { model | comment = Just newComment }

        UpdateAmount amountStr ->
            { model | amount = String.toFloat amountStr }


updateIngredientGroup : IngredientGroupMsg -> IngredientGroup -> IngredientGroup
updateIngredientGroup msg model =
    case msg of
        UpdateGroupName newName ->
            { model | name = newName }

        AddIngredient ->
            { model | ingredients = model.ingredients ++ [ Ingredient Nothing "" "" Nothing ] }

        RemoveIngredient index ->
            { model | ingredients = removeElementAt index model.ingredients }

        RoutedIngredientMsg groupIndex index submsg ->
            let
                newIngredients =
                    List.indexedMap
                        (\i ing ->
                            if i == index then
                                updateIngredient submsg ing

                            else
                                ing
                        )
                        model.ingredients
            in
            { model | ingredients = newIngredients }



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
        Failure ->
            text "that went wrong..."

        Loading ->
            text "loading..."

        ViewReceipe receipe mode ->
            case mode of
                Display ->
                    div []
                        [ h1 [] [ text receipe.title ]
                        , viewImages receipe.id receipe.image_ids
                        , p []
                            [ b []
                                [ text
                                    ("Zutaten für "
                                        ++ String.fromInt receipe.servings.amount
                                        ++ " "
                                        ++ receipe.servings.unit
                                        ++ ":"
                                    )
                                ]
                            ]
                        , div [] (List.map (viewIngredientGroup receipe.units) receipe.ingredients)
                        , button [ onClick EditReceipe ] [ text "edit" ]
                        ]

                Edit ->
                    let
                        routeMsgs =
                            Html.map (\msg -> RoutedReceipeMsg msg)
                    in
                    div []
                        [ routeMsgs (h1 [] [ text "Titel: ", input [ value receipe.title, onInput UpdateTitle ] [] ])
                        , viewImages receipe.id receipe.image_ids
                        , p []
                            [ routeMsgs
                                (b []
                                    [ text "Zutaten für "
                                    , input
                                        [ type_ "number"
                                        , value (String.fromInt receipe.servings.amount)
                                        , onInput UpdateServingsAmount
                                        ]
                                        []
                                    , text " "
                                    , input
                                        [ value receipe.servings.unit
                                        , onInput UpdateServingsUnit
                                        ]
                                        []
                                    , text ":"
                                    ]
                                )
                            ]
                        , div [] (List.indexedMap (editIngredientGroup receipe) receipe.ingredients)
                        , Html.map (\msg -> RoutedReceipeMsg msg)
                            (button [ onClick AddIngredientGroup ]
                                [ text "Zutatengruppe hinzufügen" ]
                            )
                        , button [ onClick Save ] [ text "Speichern" ]
                        , button [ onClick CancelEdit ] [ text "Abbrechen" ]
                        ]


viewImages : Int -> List Int -> Html Msg
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


viewIngredientGroup : Dict String Unit -> IngredientGroup -> Html Msg
viewIngredientGroup units ingredientGroup =
    div []
        [ b [] [ text ingredientGroup.name ]
        , ul [] (List.map (viewIngredient units) ingredientGroup.ingredients)
        ]


editIngredientGroup : Receipe -> Int -> IngredientGroup -> Html Msg
editIngredientGroup receipe i ingredientGroup =
    let
        mapMessages =
            Html.map (\msg -> RoutedReceipeMsg (RoutedIngredientGroupMsg i msg))
    in
    div []
        [ mapMessages
            (b []
                [ text "Zutatengruppe: "
                , input [ value ingredientGroup.name, onInput UpdateGroupName ] []
                ]
            )
        , Html.map (\msg -> RoutedReceipeMsg msg) (button [ onClick (RemoveIngredientGroup i) ] [ text "entfernen" ])
        , ul []
            (List.indexedMap
                (editIngredient receipe.units i ingredientGroup)
                ingredientGroup.ingredients
            )
        , mapMessages (button [ onClick AddIngredient ] [ text "Zutat hinzufügen" ])
        ]


viewIngredient : Dict String Unit -> Ingredient -> Html Msg
viewIngredient units ingredient =
    li []
        [ text (viewMaybeFloat ingredient.amount)
        , text (viewUnit ingredient.unit units)
        , text " "
        , text ingredient.name
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
    case Dict.get ingredient_unit_id units of
        Just unit ->
            unit.symbol

        Nothing ->
            "??"


editIngredient : Dict String Unit -> Int -> IngredientGroup -> Int -> Ingredient -> Html Msg
editIngredient units i ingredientGroup j ingredient =
    li []
        (List.map (Html.map (\msg -> RoutedReceipeMsg (RoutedIngredientGroupMsg i (RoutedIngredientMsg i j msg))))
            [ input
                [ type_ "number"
                , value (viewMaybeFloat ingredient.amount)
                , onInput UpdateAmount
                ]
                []
            , editUnit ingredient.unit units
            , text " "
            , input
                [ type_ "text"
                , value ingredient.name
                , onInput UpdateName
                ]
                []
            , input
                [ type_ "text"
                , value (Maybe.withDefault "" ingredient.comment)
                , onInput UpdateComment
                ]
                []
            ]
            ++ [ Html.map (\msg -> RoutedReceipeMsg (RoutedIngredientGroupMsg i msg))
                    (button [ onClick (RemoveIngredient j) ] [ text "-" ])
               ]
        )


editUnit : String -> Dict.Dict String Unit -> Html IngredientMsg
editUnit ingredient_unit_id units =
    let
        selected =
            Dict.get ingredient_unit_id units

        sortedUnits =
            case selected of
                Just unit ->
                    unit :: (Dict.remove ingredient_unit_id units |> Dict.values)

                Nothing ->
                    units |> Dict.values
    in
    select [ onInput UpdateUnit ]
        (List.map (\unit -> option [ value unit.id ] [ text unit.symbol ]) sortedUnits)



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
        (field "image_ids" (Json.Decode.list int))
        (field "servings" servingsDecoder)
        (field "ingredients" (Json.Decode.list ingredientGroupDecoder))
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
        (field "ingredients" (Json.Decode.list ingredientDecoder))


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



-- HELPERS


removeElementAt : Int -> List a -> List a
removeElementAt index list =
    List.take index list ++ List.drop (index + 1) list
