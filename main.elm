module Main exposing (..)

import Html exposing (Html, div, text, img)
import Html.Attributes exposing (style, src)
import BoardView
import WebSocket
import Model exposing (Piece)
import Position exposing (Size)
import Process
import Task
import Communication exposing (InMessage(..))
import Time exposing (second)
import Model exposing (..)
import Action exposing (Action)


main =
    Html.program { init = init, view = view, update = update, subscriptions = subscriptions }


type Msg
    = SelectField Int
    | WebSocketReceive String
    | ActionDone


init =
    Model
        None
        []
        { x = 8, y = 8 }
        ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectField index ->
            let
                ( selection, cmds ) =
                    case model.selection of
                        None ->
                            ( AvailablePending index, [ Communication.queryActions <| Position.fromIndex model.boardSize index ] )

                        AvailablePending _ ->
                            ( model.selection, [] )

                        Available selected available ->
                            let
                                maybeTarget =
                                    List.map .target available
                                        |> List.indexedMap (,)
                                        |> List.filter (\t -> Position.toIndex model.boardSize (Tuple.second t) == index)
                                        |> List.map Tuple.first
                                        |> List.head
                            in
                                case maybeTarget of
                                    Just targetIndex ->
                                        ( Selected selected index, [ Communication.execAction (Position.fromIndex model.boardSize selected) targetIndex ] )

                                    Nothing ->
                                        ( None, [] )

                        Selected origin target ->
                            ( Selected origin target, [] )
            in
                { model | selection = selection } ! cmds

        WebSocketReceive data ->
            case (Communication.handleMessage data) of
                Update pieces ->
                    { model | pieces = pieces } ! [ Task.perform (\_ -> ActionDone) (Process.sleep (1 * second)) ]

                ValidActions validActions ->
                    case model.selection of
                        None ->
                            model ! []

                        AvailablePending selected ->
                            { model | selection = Available selected validActions } ! []

                        Available _ _ ->
                            { model | selection = None } ! []

                        Selected _ _ ->
                            { model | selection = None } ! []

                Invalid e ->
                    model ! []

        ActionDone ->
            { model | selection = None } ! []


view : Model -> Html Msg
view model =
    let
        selection =
            case model.selection of
                None ->
                    { selected = -1, targets = [], selectedTarget = -1 }

                AvailablePending selected ->
                    { selected = selected, targets = [], selectedTarget = -1 }

                Available selected targets ->
                    let
                        targetIndices =
                            List.map .target targets
                                |> List.map (Debug.log "target")
                                |> List.map (Position.toIndex model.boardSize)
                                |> Debug.log "mapped"
                    in
                        { selected = selected, targets = targetIndices, selectedTarget = -1 }

                Selected selected selectedTarget ->
                    { selected = selected, targets = [], selectedTarget = selectedTarget }
    in
        div [ flexCenter, style [ ( "width", "100%" ), ( "height", "100%" ), ( "background", "#BDBDBD" ) ] ]
            [ (BoardView.draw
                { boardSize = model.boardSize
                , selected = selection.selected
                , selectedTarget = selection.selectedTarget
                , targets = selection.targets
                , pieces = model.pieces
                , onClick = SelectField
                , selection = []
                }
              )
            , Html.node
                "link"
                [ Html.Attributes.rel "stylesheet"
                , Html.Attributes.type_ "text/css"
                , Html.Attributes.href "assets/stylesheets/elm.css"
                ]
                []
            ]


updatePosition pos piece =
    { piece | pos = pos }


subscriptions model =
    WebSocket.listen "ws://localhost:9000/socket" WebSocketReceive


flexCenter =
    style
        [ ( "display", "flex" )
        , ( "justify-content", "center" )
        , ( "align-items", "center" )
        ]
