module Model exposing (..)

import Action exposing (Action)
import Position exposing (Position, Size)


type alias Model =
    { selection : Selection
    , pieces : List Piece
    , boardSize : Size
    , winner : Maybe String
    }


type Selection
    = None
    | AvailablePending Int
    | Available Int (List Action)
    | Selected Int Int


type alias Piece =
    { id : Int
    , pos : Position
    , img : String
    }


type alias Choice =
    { action : Action
    , img : String
    , description : String
    }
