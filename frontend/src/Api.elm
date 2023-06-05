module Api exposing (..)

import Receipe exposing (Receipe)
import Url.Builder as UB


type Endpoint
    = ReceipeList
    | Receipe Int
    | ReceipeImage Receipe Int
    | ImportReceipe String


toString : Endpoint -> String
toString endpoint =
    case endpoint of
        ReceipeList ->
            UB.absolute [ "receipe", "list" ] []

        Receipe id ->
            UB.absolute [ "receipe" ] [ UB.int "id" id ]

        ReceipeImage receipe image_id ->
            UB.absolute [ "receipe", "image" ] [ UB.int "receipe_id" receipe.id, UB.int "image_id" image_id ]

        ImportReceipe url ->
            UB.absolute [ "receipe", "import" ] [ UB.string "url" url ]
