module Communication exposing (..)

import Json.Decode exposing (decodeString, field, list, map2, string, nullable, oneOf, map, at, maybe)
import Json.Encode as Encode
import ComPiece
import Position
import Model exposing (Piece)
import Action exposing (Action)
import Model exposing (Model)
import WebSocket
import Position exposing (Position)


type InMessage
    = Update (List Piece) (Maybe String)
    | ValidActions (List Action)
    | Invalid String


handleMessage : String -> InMessage
handleMessage msg =
    case decodeString inMessageDecoder (Debug.log "JSON:" msg) of
        Ok inMessage ->
            inMessage

        Err e ->
            Invalid <| Debug.log "Error" e


queryActionsCmd : Position -> Cmd msg
queryActionsCmd pos =
    Encode.object
        [ ( "origin", Position.encode pos ) ]
        |> Encode.encode 0
        |> WebSocket.send "ws://localhost:9000/socket"


execActionCmd : Position -> Int -> Cmd msg
execActionCmd pos index =
    Encode.object
        [ ( "position", Position.encode pos )
        , ( "index", Encode.int index )
        ]
        |> Encode.encode 0
        |> WebSocket.send "ws://localhost:9000/socket"


inMessageDecoder : Json.Decode.Decoder InMessage
inMessageDecoder =
    oneOf [ validActionsDecoder, updateDecoder ]


updateDecoder : Json.Decode.Decoder InMessage
updateDecoder =
    map2 Update
        (at [ "chessBoard", "pieces" ] (list posPieceDecoder))
        (maybe (field "winner" string))


validActionsDecoder : Json.Decode.Decoder InMessage
validActionsDecoder =
    map ValidActions
        (list Action.decoder)


posPieceDecoder : Json.Decode.Decoder Piece
posPieceDecoder =
    map2 ComPiece.toModelPiece
        (field "piece" ComPiece.decoder)
        (field "pos" Position.decoder)
