module Api exposing (..)

import Receipe exposing (ReceipeID)
import Url.Builder as UB
import Http


type Endpoint
    = ReceipeList
    | Receipe Int
    | ReceipeImage ReceipeID Int
    | ImportReceipe String
    | DeleteReceipe ReceipeID
    | CreateReceipe


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

        DeleteReceipe id ->
            UB.absolute [ "receipe", "delete" ] [ UB.int "id" id ]

        CreateReceipe ->
            UB.absolute [ "receipe", "create" ] []


-- HTTP


deleteReceipe : (Result Http.Error String -> a) -> Int -> Cmd a
deleteReceipe msg id =
    post
        { endpoint = DeleteReceipe id
        , body = Http.emptyBody
        , expect = Http.expectString msg
        }


get : { endpoint: Endpoint, expect: Http.Expect msg } -> Cmd msg
get arg =
    Http.get
        { url = arg.endpoint |> toString
        , expect = arg.expect
        }

post : { endpoint: Endpoint, expect: Http.Expect msg, body: Http.Body } -> Cmd msg
post arg =
    Http.post
        { url = arg.endpoint |> toString
        , body = arg.body
        , expect = arg.expect
        }
