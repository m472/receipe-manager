module ReceipeEditor exposing (..)

import Browser.Navigation as Nav
import Dict exposing (Dict)
import Helpers
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Receipe
import ReceipeImageViewer
import Route



-- MODEL


type alias Model =
    { receipe : Receipe.Receipe
    , currentImage : Int
    }


type Msg
    = AddIngredientGroup
    | RemoveIngredientGroup Int
    | UpdateTitle String
    | UpdateServingsAmount String
    | UpdateServingsUnit String
    | RoutedIngredientGroupMsg Int IngredientGroupMsg
    | Uploaded (Result Http.Error ())
    | Save
    | CancelEdit
    | ImageViewerMsg ReceipeImageViewer.Msg


type IngredientMsg
    = UpdateName String
    | UpdateUnit String
    | UpdateComment String
    | UpdateAmount String


type IngredientGroupMsg
    = UpdateGroupName String
    | AddIngredient
    | RemoveIngredient Int
    | RoutedIngredientMsg Int IngredientMsg



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Titel: ", input [ value model.receipe.title, onInput UpdateTitle ] [] ]
        , Html.map ImageViewerMsg (ReceipeImageViewer.viewImages model.receipe model.currentImage)
        , p []
            [ b []
                [ text "Zutaten für "
                , input
                    [ type_ "number"
                    , value (String.fromInt model.receipe.servings.amount)
                    , onInput UpdateServingsAmount
                    ]
                    []
                , text " "
                , input
                    [ value model.receipe.servings.unit
                    , onInput UpdateServingsUnit
                    ]
                    []
                , text ":"
                ]
            ]
        , div [] (List.indexedMap (editIngredientGroup model.receipe) model.receipe.ingredients)
        , button [ onClick AddIngredientGroup ]
            [ text "Zutatengruppe hinzufügen" ]
        , button [ onClick Save ] [ text "Speichern" ]
        , button [ onClick CancelEdit ] [ text "Abbrechen" ]
        ]


editIngredientGroup : Receipe.Receipe -> Int -> Receipe.IngredientGroup -> Html Msg
editIngredientGroup receipe i ingredientGroup =
    let
        mapMessages =
            Html.map (RoutedIngredientGroupMsg i)
    in
    div []
        [ mapMessages
            (b []
                [ text "Zutatengruppe: "
                , input [ value ingredientGroup.name, onInput UpdateGroupName ] []
                ]
            )
        , button [ onClick (RemoveIngredientGroup i) ] [ text "entfernen" ]
        , ul []
            (List.indexedMap
                (editIngredient receipe.units i)
                ingredientGroup.ingredients
            )
        , mapMessages (button [ onClick AddIngredient ] [ text "Zutat hinzufügen" ])
        ]


editIngredient : Dict String Receipe.Unit -> Int -> Int -> Receipe.Ingredient -> Html Msg
editIngredient units i j ingredient =
    li []
        (List.map (Html.map (\msg -> RoutedIngredientGroupMsg i (RoutedIngredientMsg j msg)))
            [ input
                [ type_ "number"
                , value (Helpers.viewMaybeFloat 1 ingredient.amount)
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
            ++ [ Html.map (RoutedIngredientGroupMsg i)
                    (button [ onClick (RemoveIngredient j) ] [ text "-" ])
               ]
        )


editUnit : String -> Dict.Dict String Receipe.Unit -> Html IngredientMsg
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



-- UPDATE


updateReceipe : Msg -> Model -> ( Model, Cmd Msg )
updateReceipe msg model =
    let
        receipe =
            model.receipe
    in
    case msg of
        AddIngredientGroup ->
            ( { model | receipe = { receipe | ingredients = receipe.ingredients ++ [ Receipe.IngredientGroup "" [] ] } }, Cmd.none )

        UpdateTitle newName ->
            ( { model | receipe = { receipe | title = newName } }, Cmd.none )

        UpdateServingsUnit newUnit ->
            let
                servs =
                    receipe.servings
            in
            ( { model | receipe = { receipe | servings = { servs | unit = newUnit } } }, Cmd.none )

        UpdateServingsAmount amountStr ->
            let
                servs =
                    receipe.servings

                newAmount =
                    String.toInt amountStr
            in
            case newAmount of
                Just value ->
                    ( { model | receipe = { receipe | servings = { servs | amount = value } } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        RemoveIngredientGroup index ->
            ( { model | receipe = { receipe | ingredients = Helpers.removeElementAt index receipe.ingredients } }, Cmd.none )

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
                        receipe.ingredients
            in
            ( { model | receipe = { receipe | ingredients = groups } }, Cmd.none )

        Uploaded _ ->
            ( model, Route.load (Route.ViewReceipe receipe.id) )

        CancelEdit ->
            ( model, Route.load (Route.ViewReceipe receipe.id) )

        Save ->
            ( model, sendReceipe receipe )

        ImageViewerMsg subMsg ->
            ( { model
                | currentImage =
                    ReceipeImageViewer.update subMsg receipe model.currentImage
              }
            , Cmd.none
            )


updateIngredient : IngredientMsg -> Receipe.Ingredient -> Receipe.Ingredient
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


updateIngredientGroup : IngredientGroupMsg -> Receipe.IngredientGroup -> Receipe.IngredientGroup
updateIngredientGroup msg model =
    case msg of
        UpdateGroupName newName ->
            { model | name = newName }

        AddIngredient ->
            { model | ingredients = model.ingredients ++ [ Receipe.Ingredient Nothing "" "" Nothing ] }

        RemoveIngredient index ->
            { model | ingredients = Helpers.removeElementAt index model.ingredients }

        RoutedIngredientMsg index submsg ->
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


sendReceipe : Receipe.Receipe -> Cmd Msg
sendReceipe receipe =
    Http.post
        { url = "/receipe/update?id=" ++ String.fromInt receipe.id
        , body = Http.jsonBody (Receipe.encoder receipe)
        , expect = Http.expectWhatever Uploaded
        }
