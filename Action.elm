module Action exposing (..)

import Position exposing (Position)
import Json.Decode exposing (field, string, map3, nullable)


type alias Action =
    { name : String
    , target : Position
    , choice : Maybe String
    }


decoder : Json.Decode.Decoder Action
decoder =
    map3 Action
        (field "name" string)
        (field "target" Position.decoder)
        (field "choice" (nullable string))
