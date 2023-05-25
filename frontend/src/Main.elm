module Main exposing (..)

import Browser
import Browser.Navigation
import Debug
import Dict exposing (Dict)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Decimals(..), usLocale)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as JD
import Json.Encode as JE
import Url



-- MAIN


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


type Model
    = Failure
    | Loading
    | ViewReceipe ScaledReceipe Mode String
    | ViewReceipeList (List ReceipePreview)


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url nav =
    ( Loading, getReceipeList )


type alias ScaledReceipe =
    { receipe : Receipe
    , servings : Int
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


type alias ReceipePreview =
    { id : Int
    , title : String
    , image_ids : List Int
    }



-- UPDATE


type Msg
    = GotReceipe (Result Http.Error Receipe)
    | GotReceipeList (Result Http.Error (List ReceipePreview))
    | EditReceipe
    | Save
    | CancelEdit
    | ReceipeServingsChanged String
    | RoutedReceipeMsg ReceipeMsg
    | Uploaded (Result Http.Error ())
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


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
                    ( ViewReceipe (ScaledReceipe receipe receipe.servings.amount) Display "", Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )

        GotReceipeList result ->
            case result of
                Ok receipes ->
                    ( ViewReceipeList receipes, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )

        EditReceipe ->
            case model of
                ViewReceipe receipe _ info ->
                    ( ViewReceipe receipe Edit info, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Save ->
            case model of
                ViewReceipe scaled_receipe Edit info ->
                    ( ViewReceipe scaled_receipe Display info, sendReceipe scaled_receipe.receipe )

                _ ->
                    ( model, Cmd.none )

        CancelEdit ->
            ( Loading
            , getReceipe
            )

        ReceipeServingsChanged servingsStr ->
            let
                servingsFactor =
                    String.toInt servingsStr
            in
            case ( servingsFactor, model ) of
                ( Just value, ViewReceipe receipe Display info ) ->
                    ( ViewReceipe { receipe | servings = value } Display info
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        RoutedReceipeMsg childMsg ->
            case model of
                ViewReceipe scaled_receipe Edit info ->
                    ( ViewReceipe { scaled_receipe | receipe = updateReceipe childMsg scaled_receipe.receipe } Edit info
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        Uploaded _ ->
            ( model, Cmd.none )

        UrlChanged url ->
            ( model, Cmd.none )

        LinkClicked url ->
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


sendReceipe : Receipe -> Cmd Msg
sendReceipe receipe =
    Http.post
        { url = "/receipe/update?id=0"
        , body = Http.jsonBody (receipeEncoder receipe)
        , expect = Http.expectWhatever Uploaded
        }



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
            [ case model of
                Failure ->
                    text "that went wrong..."

                Loading ->
                    text "loading..."

                ViewReceipe receipe mode msg ->
                    case mode of
                        Display ->
                            viewReceipe receipe

                        Edit ->
                            editReceipe receipe.receipe

                ViewReceipeList receipeList ->
                    viewReceipeList receipeList
            ]
        ]


viewReceipeList : List ReceipePreview -> Html Msg
viewReceipeList receipeList =
    div []
        [ h1 [] [ text "Rezepte-Übersicht" ]
        , ul []
            (List.map
                (\receipe ->
                    li []
                        [ a [ href ("/receipe?id=" ++ String.fromInt receipe.id) ]
                            [ text receipe.title ]
                        ]
                )
                receipeList
            )
        ]


viewReceipe : ScaledReceipe -> Html Msg
viewReceipe scaled_receipe =
    let
        receipe =
            scaled_receipe.receipe

        scaling_factor =
            toFloat scaled_receipe.servings / toFloat receipe.servings.amount
    in
    div []
        [ h1 [] [ text receipe.title ]
        , viewImages receipe.id receipe.image_ids
        , p []
            [ b []
                [ text "Zutaten für "
                , input
                    [ type_ "number"
                    , value (String.fromInt scaled_receipe.servings)
                    , onInput ReceipeServingsChanged
                    ]
                    []
                , text (" " ++ receipe.servings.unit ++ ":")
                ]
            ]
        , div [] (List.map (viewIngredientGroup scaling_factor receipe.units) receipe.ingredients)
        , button [ onClick EditReceipe ] [ text "edit" ]
        ]


editReceipe : Receipe -> Html Msg
editReceipe receipe =
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


viewIngredientGroup : Float -> Dict String Unit -> IngredientGroup -> Html Msg
viewIngredientGroup factor units ingredientGroup =
    div []
        [ b [] [ text ingredientGroup.name ]
        , ul [] (List.map (viewIngredient factor units) ingredientGroup.ingredients)
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


viewIngredient : Float -> Dict String Unit -> Ingredient -> Html Msg
viewIngredient factor units ingredient =
    li []
        [ text (viewMaybeFloat factor ingredient.amount)
        , text (viewUnit ingredient.unit units)
        , text " "
        , text ingredient.name
        , text " "
        , text (Maybe.withDefault "" ingredient.comment)
        ]


viewMaybeFloat : Float -> Maybe Float -> String
viewMaybeFloat factor value =
    case value of
        Just floatValue ->
            format { usLocale | decimals = Max 1, thousandSeparator = "'" } (factor * floatValue)

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
                , value (viewMaybeFloat 1 ingredient.amount)
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


getReceipe : Cmd Msg
getReceipe =
    Http.get
        { url = "/receipe?id=0"
        , expect = Http.expectJson GotReceipe receipeDecoder
        }


receipeDecoder : JD.Decoder Receipe
receipeDecoder =
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



-- ENCODERS


receipeEncoder : Receipe -> JE.Value
receipeEncoder receipe =
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



-- HELPERS


removeElementAt : Int -> List a -> List a
removeElementAt index list =
    List.take index list ++ List.drop (index + 1) list
