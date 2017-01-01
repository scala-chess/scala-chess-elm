module ComPiece exposing (..)

import Json.Decode as Decode exposing (..)
import Json.Encode as Encode


type alias ComPiece =
    { id : Int
    , name : String
    , color : String
    }


decoder =
    map3 ComPiece
        (field "id" int)
        (field "name" string)
        (field "color" string)


encoder piece =
    Encode.object
        [ ( "id", Encode.int piece.id )
        , ( "name", Encode.string piece.name )
        , ( "color", Encode.string piece.color )
        ]


img piece =
    "assets/pieces/" ++ piece.color ++ "_" ++ piece.name ++ ".png"
