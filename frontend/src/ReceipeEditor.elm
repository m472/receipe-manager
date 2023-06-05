module ReceipeEditor exposing (..)

import Dict exposing (Dict)
import File exposing (File)
import Helpers
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as JD
import Platform.Cmd as Cmd
import Receipe exposing (InstructionGroup)
import ReceipeImageViewer
import Route
import Url.Builder



-- MODEL


type alias Model =
    { receipe : Receipe.Receipe
    , currentImage : Int
    , errorMessage : String
    }


type Msg
    = AddIngredientGroup
    | RemoveIngredientGroup Int
    | UpdateTitle String
    | UpdateServingsAmount String
    | UpdateServingsUnit String
    | RoutedIngredientGroupMsg Int IngredientGroupMsg
    | RoutedInstructionMsg Int InstructionGroupMsg
    | ReceipeUploaded (Result Http.Error ())
    | ImageUploaded (Result Http.Error ())
    | Save
    | CancelEdit
    | ImageViewerMsg ReceipeImageViewer.Msg
    | GotImages (List File)
    | DeleteImage
    | RemoveInstructionGroup Int


type IngredientMsg
    = UpdateIngredientName String
    | UpdateUnit String
    | UpdateComment String
    | UpdateAmount String


type InstructionGroupMsg
    = UpdateInstructionGroupName String
    | UpdateStep Int String
    | AddStep
    | RemoveStep Int


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
        , input [ on "change" (JD.map GotImages filesDecoder), type_ "file", multiple True ]
            [ text "Bild auswählen" ]
        , button [ onClick DeleteImage ] [ text "Bild löschen" ]
        , h2 [] [ text "Zutaten" ]
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
        , div [] (List.indexedMap (viewIngredientGroup model.receipe) model.receipe.ingredients)
        , button [ onClick AddIngredientGroup ]
            [ text "Zutatengruppe hinzufügen" ]
        , h2 [] [ text "Zubereitung" ]
        , div []
            (List.indexedMap
                (\i ig -> viewInstructionGroup i ig)
                model.receipe.instructions
            )
        , button [ onClick Save ] [ text "Speichern" ]
        , button [ onClick CancelEdit ] [ text "Abbrechen" ]
        ]


viewIngredientGroup : Receipe.Receipe -> Int -> Receipe.IngredientGroup -> Html Msg
viewIngredientGroup receipe i ingredientGroup =
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
                (viewIngredient receipe.units i)
                ingredientGroup.ingredients
            )
        , mapMessages (button [ onClick AddIngredient ] [ text "Zutat hinzufügen" ])
        ]


viewIngredient : Dict String Receipe.Unit -> Int -> Int -> Receipe.Ingredient -> Html Msg
viewIngredient units i j ingredient =
    li []
        (List.map (Html.map (\msg -> RoutedIngredientGroupMsg i (RoutedIngredientMsg j msg)))
            [ input
                [ type_ "number"
                , value (Helpers.viewMaybeFloat 1 ingredient.amount)
                , onInput UpdateAmount
                ]
                []
            , viewUnit ingredient.unit units
            , text " "
            , input
                [ type_ "text"
                , value ingredient.name
                , onInput UpdateIngredientName
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


viewUnit : String -> Dict.Dict String Receipe.Unit -> Html IngredientMsg
viewUnit ingredient_unit_id units =
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


viewInstructionGroup : Int -> InstructionGroup -> Html Msg
viewInstructionGroup i instructionGroup =
    div []
        (b []
            [ text "Zubereitungsschritte für "
            , Html.map (RoutedInstructionMsg i)
                (input
                    [ type_ "text"
                    , onInput UpdateInstructionGroupName
                    , value instructionGroup.name
                    ]
                    []
                )
            , text ":"
            ]
            :: List.indexedMap
                (\j s -> Html.map (RoutedInstructionMsg i) (viewInstructionStep j s))
                instructionGroup.steps
            ++ [ Html.map (RoutedInstructionMsg i) (button [ onClick AddStep ] [ text "Schritt hinzufügen" ])
               , button [ onClick (RemoveInstructionGroup i) ]
                    [ text "Zubereitungsgruppe entfernen" ]
               ]
        )


viewInstructionStep : Int -> String -> Html InstructionGroupMsg
viewInstructionStep index step =
    div []
        [ text (String.fromInt (index + 1) ++ ". ")
        , textarea [ onInput (UpdateStep index) ] [ text step ]
        , button [ onClick (RemoveStep index) ] [ text "-" ]
        ]



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
                    ( { model
                        | receipe =
                            { receipe
                                | servings = { servs | amount = value }
                            }
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        RemoveIngredientGroup index ->
            ( { model
                | receipe =
                    { receipe
                        | ingredients = Helpers.removeElementAt index receipe.ingredients
                    }
              }
            , Cmd.none
            )

        RoutedIngredientGroupMsg groupIndex childMsg ->
            let
                groups =
                    Helpers.updateElementAt groupIndex
                        (updateIngredientGroup childMsg)
                        receipe.ingredients
            in
            ( { model | receipe = { receipe | ingredients = groups } }, Cmd.none )

        RoutedInstructionMsg groupIndex childMsg ->
            let
                groups =
                    Helpers.updateElementAt groupIndex
                        (updateInstructionGroup childMsg)
                        receipe.instructions
            in
            ( { model
                | receipe =
                    { receipe
                        | instructions = groups
                    }
              }
            , Cmd.none
            )

        ReceipeUploaded _ ->
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

        GotImages images ->
            case images of
                [] ->
                    ( model, Cmd.none )

                head :: tail ->
                    let
                        ( r, firstCmd ) =
                            uploadImage head model.receipe

                        ( updatedModel, secondCmd ) =
                            updateReceipe (GotImages tail) { model | receipe = r }
                    in
                    ( updatedModel, Cmd.batch [ firstCmd, secondCmd ] )

        ImageUploaded result ->
            case result of
                Ok _ ->
                    ( model, Cmd.none )

                Err _ ->
                    ( { model | errorMessage = "shit..." }, Cmd.none )

        DeleteImage ->
            let
                updatedImageIds =
                    Helpers.removeElementAt
                        model.currentImage
                        model.receipe.image_ids

                wasUpdated =
                    List.length updatedImageIds
                        /= List.length model.receipe.image_ids
            in
            ( Debug.log ("delete image " ++ String.fromInt model.currentImage)
                { model
                    | receipe = { receipe | image_ids = updatedImageIds }
                    , currentImage =
                        model.currentImage
                            - (if wasUpdated then
                                1

                               else
                                0
                              )
                }
            , Cmd.none
            )

        RemoveInstructionGroup i ->
            ( { model
                | receipe =
                    { receipe
                        | ingredients = Helpers.removeElementAt i model.receipe.ingredients
                    }
              }
            , Cmd.none
            )


updateIngredient : IngredientMsg -> Receipe.Ingredient -> Receipe.Ingredient
updateIngredient msg model =
    case msg of
        UpdateIngredientName newName ->
            { model | name = newName }

        UpdateUnit newUnitId ->
            { model | unit = newUnitId }

        UpdateComment newComment ->
            { model | comment = Just newComment }

        UpdateAmount amountStr ->
            { model | amount = String.toFloat amountStr }


updateIngredientGroup :
    IngredientGroupMsg
    -> Receipe.IngredientGroup
    -> Receipe.IngredientGroup
updateIngredientGroup msg model =
    case msg of
        UpdateGroupName newName ->
            { model | name = newName }

        AddIngredient ->
            { model
                | ingredients =
                    model.ingredients
                        ++ [ Receipe.Ingredient Nothing "" "" Nothing ]
            }

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


updateInstructionGroup :
    InstructionGroupMsg
    -> Receipe.InstructionGroup
    -> Receipe.InstructionGroup
updateInstructionGroup msg model =
    case msg of
        UpdateStep index value ->
            { model | steps = Helpers.updateElementAt index (\_ -> value) model.steps }

        AddStep ->
            { model | steps = model.steps ++ [ "" ] }

        RemoveStep index ->
            { model | steps = Helpers.removeElementAt index model.steps }

        UpdateInstructionGroupName name ->
            { model | name = name }


sendReceipe : Receipe.Receipe -> Cmd Msg
sendReceipe receipe =
    Http.post
        { url =
            Url.Builder.absolute [ "receipe", "update" ]
                [ Url.Builder.int "id" receipe.id ]
        , body = Http.jsonBody (Receipe.encoder receipe)
        , expect = Http.expectWhatever ReceipeUploaded
        }


uploadImage : File -> Receipe.Receipe -> ( Receipe.Receipe, Cmd Msg )
uploadImage file receipe =
    let
        newImgId =
            List.length receipe.image_ids
    in
    ( { receipe | image_ids = newImgId :: receipe.image_ids }
    , Http.request
        { method = "PUT"
        , headers = []
        , url =
            Url.Builder.absolute [ "receipe", "image", "upload" ]
                [ Url.Builder.int "receipe_id" receipe.id
                , Url.Builder.int "image_id" newImgId
                ]
        , body = Http.fileBody file
        , expect = Http.expectWhatever ImageUploaded
        , timeout = Nothing
        , tracker = Nothing
        }
    )



-- DECODER


filesDecoder : JD.Decoder (List File)
filesDecoder =
    JD.at [ "target", "files" ] (JD.list File.decoder)
