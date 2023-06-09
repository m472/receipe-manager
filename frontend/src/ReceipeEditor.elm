module ReceipeEditor exposing (..)

import Dict exposing (Dict)
import File exposing (File)
import Helpers exposing (lift)
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
    { receipe : EditableReceipe
    , currentImage : Int
    , errorMessage : String
    }


type alias EditableReceipe =
    { id : Receipe.ReceipeID
    , title : String
    , image_ids : List Int
    , servings : EditableServings
    , ingredients : List EditableIngredientGroup
    , instructions : List InstructionGroup
    , units : Dict String Receipe.Unit
    , tags : String
    }


type alias EditableServings =
    { amount : Result (ParseError Int) Int
    , unit : String
    }


type alias EditableIngredientGroup =
    { name : String
    , ingredients : List EditableIngredient
    }


type alias EditableIngredient =
    { amount : Result (ParseError (Maybe Float)) (Maybe Float)
    , unit : String
    , name : String
    , comment : Maybe String
    }


type alias ParseError a =
    { message : String
    , lastValidValue : a
    , invalidValue : String
    }


type Msg
    = AddIngredientGroup
    | RemoveIngredientGroup Int
    | UpdateTitle String
    | UpdateServingsAmount String
    | UpdateServingsUnit String
    | UpdateTags String
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
        , input
            [ type_ "text"
            , value model.receipe.tags
            , onInput UpdateTags
            ]
            []
        , div [] (splitTags model.receipe.tags |> List.map (\s -> " " ++ s ++ " " |> text))
        , Html.map ImageViewerMsg (ReceipeImageViewer.viewImages model.receipe.id model.receipe.image_ids model.currentImage)
        , input [ on "change" (JD.map GotImages filesDecoder), type_ "file", multiple True ]
            [ text "Bild auswählen" ]
        , button [ onClick DeleteImage ] [ text "Bild löschen" ]
        , h2 [] [ text "Zutaten" ]
        , p []
            [ b []
                [ text "Zutaten für "
                , viewEditInt UpdateServingsAmount model.receipe.servings.amount
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
        , br [] []
        , text model.errorMessage
        , br [] []
        , button [ onClick Save ] [ text "Speichern" ]
        , button [ onClick CancelEdit ] [ text "Abbrechen" ]
        ]


viewIngredientGroup : EditableReceipe -> Int -> EditableIngredientGroup -> Html Msg
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


viewIngredient : Dict String Receipe.Unit -> Int -> Int -> EditableIngredient -> Html Msg
viewIngredient units i j ingredient =
    li []
        (List.map (Html.map (\msg -> RoutedIngredientGroupMsg i (RoutedIngredientMsg j msg)))
            [ viewEditMaybeFloat UpdateAmount ingredient.amount
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


viewEditFloat : (String -> b) -> Result (ParseError Float) Float -> Html b
viewEditFloat onInputMsg =
    viewEditNumber onInputMsg String.fromFloat


viewEditInt : (String -> b) -> Result (ParseError Int) Int -> Html b
viewEditInt onInputMsg =
    viewEditNumber onInputMsg String.fromInt


viewEditMaybeFloat : (String -> b) -> Result (ParseError (Maybe Float)) (Maybe Float) -> Html b
viewEditMaybeFloat onInputMsg =
    viewEditNumber
        onInputMsg
        (\maybeVal ->
            case maybeVal of
                Just value ->
                    String.fromFloat value

                Nothing ->
                    ""
        )


viewEditNumber : (String -> b) -> (a -> String) -> Result (ParseError a) a -> Html b
viewEditNumber onInputMsg fromNumber parseResult =
    let
        ( val, msg ) =
            case parseResult of
                Ok parsed ->
                    ( fromNumber parsed, text "" )

                Err errorInfo ->
                    ( errorInfo.invalidValue, text errorInfo.message )
    in
    div []
        [ input
            [ type_ "text"
            , value val
            , onInput onInputMsg
            ]
            []
        , msg
        ]


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
            ( { model | receipe = { receipe | ingredients = receipe.ingredients ++ [ EditableIngredientGroup "" [] ] } }, Cmd.none )

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
                    case String.toInt amountStr of
                        Just value ->
                            Ok value

                        Nothing ->
                            Err { message = "Diese Eingabe konnte nicht in eine Zahl konvertiert werden", lastValidValue = getLastValid model.receipe.servings.amount, invalidValue = amountStr }
            in
            ( { model
                | receipe =
                    { receipe
                        | servings = { servs | amount = newAmount }
                    }
              }
            , Cmd.none
            )

        UpdateTags tagsStr ->
            ( { model
                | receipe = { receipe | tags = tagsStr }
              }
            , Cmd.none
            )

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
            case fromEditable receipe of
                Just convertedReceipe ->
                    ( model, sendReceipe convertedReceipe )

                Nothing ->
                    ( { model | errorMessage = "Vor dem Speichern müssen alle Fehler bereinigt werden" }, Cmd.none )

        ImageViewerMsg subMsg ->
            ( { model
                | currentImage =
                    ReceipeImageViewer.update subMsg receipe.image_ids model.currentImage
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


updateIngredient : IngredientMsg -> EditableIngredient -> EditableIngredient
updateIngredient msg model =
    let
        lastValid =
            case model.amount of
                Ok valid ->
                    valid

                Err parseError ->
                    parseError.lastValidValue
    in
    case msg of
        UpdateIngredientName newName ->
            { model | name = newName }

        UpdateUnit newUnitId ->
            { model | unit = newUnitId }

        UpdateComment newComment ->
            { model | comment = Just newComment }

        UpdateAmount amountStr ->
            { model
                | amount =
                    case ( amountStr, String.toFloat amountStr ) of
                        ( "", _ ) ->
                            Ok Nothing

                        ( _, Just value ) ->
                            Ok (Just value)

                        ( _, Nothing ) ->
                            Err { message = "Diese Eingabe konnte nicht in eine Zahl konvertiert werden", lastValidValue = lastValid, invalidValue = amountStr }
            }


updateIngredientGroup :
    IngredientGroupMsg
    -> EditableIngredientGroup
    -> EditableIngredientGroup
updateIngredientGroup msg model =
    case msg of
        UpdateGroupName newName ->
            { model | name = newName }

        AddIngredient ->
            { model
                | ingredients =
                    model.ingredients
                        ++ [ EditableIngredient (Ok Nothing) "" "" Nothing ]
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


uploadImage : File -> EditableReceipe -> ( EditableReceipe, Cmd Msg )
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



-- CONVERTER


toEditable : Receipe.Receipe -> EditableReceipe
toEditable receipe =
    { id = receipe.id
    , title = receipe.title
    , image_ids = receipe.image_ids
    , ingredients = List.map toEditableIngredientGroup receipe.ingredients
    , instructions = receipe.instructions
    , servings = { amount = Ok receipe.servings.amount, unit = receipe.servings.unit }
    , units = receipe.units
    , tags = List.intersperse ", " receipe.tags |> String.concat
    }


toEditableIngredientGroup : Receipe.IngredientGroup -> EditableIngredientGroup
toEditableIngredientGroup ig =
    { name = ig.name
    , ingredients = List.map toEditableIngredient ig.ingredients
    }


toEditableIngredient : Receipe.Ingredient -> EditableIngredient
toEditableIngredient ing =
    { amount = Ok ing.amount
    , name = ing.name
    , unit = ing.unit
    , comment = ing.comment
    }


fromEditable : EditableReceipe -> Maybe Receipe.Receipe
fromEditable receipe =
    let
        maybeInstr =
            Debug.log ("Tags: " ++ receipe.tags)
                List.map
                fromEditableIngredientGroup
                receipe.ingredients
                |> lift

        maybeServings =
            fromEditableServings receipe.servings

        tags =
            splitTags receipe.tags
    in
    case ( maybeInstr, maybeServings ) of
        ( Just ingredients, Just servings ) ->
            Just
                { id = receipe.id
                , title = receipe.title
                , image_ids = receipe.image_ids
                , ingredients = ingredients
                , instructions = receipe.instructions
                , servings = servings
                , units = receipe.units
                , tags = tags
                }

        ( _, _ ) ->
            Nothing


fromEditableIngredientGroup : EditableIngredientGroup -> Maybe Receipe.IngredientGroup
fromEditableIngredientGroup ig =
    let
        ingredients =
            List.map fromEditableIngredient ig.ingredients |> lift
    in
    Maybe.map (\ing -> { name = ig.name, ingredients = ing }) ingredients


fromEditableIngredient : EditableIngredient -> Maybe Receipe.Ingredient
fromEditableIngredient ing =
    case ing.amount of
        Err _ ->
            Nothing

        Ok value ->
            Just
                { amount = value
                , name = ing.name
                , unit = ing.unit
                , comment = ing.comment
                }


fromEditableServings : EditableServings -> Maybe Receipe.Servings
fromEditableServings s =
    case s.amount of
        Err _ ->
            Nothing

        Ok value ->
            Just
                { amount = value
                , unit = s.unit
                }



-- HELPERS


splitTags : String -> List String
splitTags tagStr =
    String.split "," tagStr |> List.map String.trim |> List.filter (not << String.isEmpty)


getLastValid : Result (ParseError a) a -> a
getLastValid result =
    case result of
        Ok value ->
            value

        Err errorInfo ->
            errorInfo.lastValidValue
