module Communication exposing (..)

import Json.Decode exposing (decodeString, field, list, map2, string, nullable, oneOf, map)
import Json.Encode as Encode
import ComPiece
import Position
import Model exposing (Piece)
import Action exposing (Action)
import Model exposing (Model)
import WebSocket


type InMessage
    = Update (List Piece)
    | ValidActions (List Action)
    | Invalid String


handleMessage : String -> InMessage
handleMessage msg =
    case decodeString inMessageDecoder msg of
        Ok inMessage ->
            Debug.log "Message" inMessage

        Err e ->
            let
                x =
                    Debug.log "Error" e
            in
                Invalid e


queryActions pos =
    WebSocket.send "ws://localhost:9000/socket"
        (Encode.object [ ( "origin", Position.encode pos ) ]
            |> Encode.encode 0
        )


execAction pos index =
    WebSocket.send "ws://localhost:9000/socket"
        (Encode.object [ ( "position", Position.encode pos ), ( "index", Encode.int index ) ]
            |> Encode.encode 0
        )


inMessageDecoder : Json.Decode.Decoder InMessage
inMessageDecoder =
    oneOf [ validActionsDecoder, updateDecoder ]


updateDecoder : Json.Decode.Decoder InMessage
updateDecoder =
    map Update
        (field "pieces" (list posPieceDecoder))


validActionsDecoder : Json.Decode.Decoder InMessage
validActionsDecoder =
    map ValidActions
        (list Action.decoder)


posPieceDecoder : Json.Decode.Decoder Piece
posPieceDecoder =
    map2 modelPiece
        (field "piece" ComPiece.decoder)
        (field "pos" Position.decoder)


modelPiece piece pos =
    Piece piece.id pos (ComPiece.img piece)
