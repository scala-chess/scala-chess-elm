module Message exposing (..)


type Msg
    = SelectField Int
    | SelectChoice String
    | WebSocketReceive String
    | ActionDone
