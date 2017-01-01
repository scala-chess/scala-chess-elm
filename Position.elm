module Position exposing (fromIndex, toIndex, Position, Size, decoder, encode)

import Json.Decode exposing (..)
import Json.Encode as Encode


type alias Position =
    { x : Int
    , y : Int
    }


type alias Size =
    Position


fromIndex : Size -> Int -> Position
fromIndex boardSize index =
    let
        x =
            index % boardSize.x

        y =
            index // boardSize.x
    in
        { x = x, y = y }


toIndex : Size -> Position -> Int
toIndex boardSize pos =
    pos.x + pos.y * boardSize.x


decoder : Decoder Position
decoder =
    map2 Position
        (field "x" int)
        (field "y" int)


encode : Position -> Encode.Value
encode pos =
    Encode.object
        [ ( "x", Encode.int pos.x )
        , ( "y", Encode.int pos.y )
        ]
