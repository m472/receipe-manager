module Api exposing (..)

import Url.Builder as UB

type Endpoint
    = ReceipeList
    | Receipe Int

toString : Endpoint -> String
toString endpoint =
    case endpoint of
        ReceipeList -> UB.absolute ["receipe", "list"] []
        Receipe id -> UB.absolute ["receipe"] [UB.int "id" id]
