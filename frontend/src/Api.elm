module Api exposing (..)

import Url.Builder as UB
import Receipe exposing (ReceipeID)


type Endpoint
    = ReceipeList
    | Receipe Int
    | ReceipeImage ReceipeID Int
    | ImportReceipe String


toString : Endpoint -> String
toString endpoint =
    case endpoint of
        ReceipeList ->
            UB.absolute [ "receipe", "list" ] []

        Receipe id ->
            UB.absolute [ "receipe" ] [ UB.int "id" id ]

        ReceipeImage receipe_id image_id ->
            UB.absolute [ "receipe", "image" ] [ UB.int "receipe_id" receipe_id, UB.int "image_id" image_id ]

        ImportReceipe url ->
            UB.absolute [ "receipe", "import" ] [ UB.string "url" url ]
