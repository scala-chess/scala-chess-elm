module Message exposing (..)


type Msg
    = SelectField Int
    | WebSocketReceive String
    | ActionDone
