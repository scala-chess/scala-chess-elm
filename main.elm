module Main exposing (..)

import Html exposing (Html)
import WebSocket
import Model exposing (Piece)
import Position exposing (Size)
import Process
import Task
import Communication exposing (InMessage(..))
import Time exposing (second)
import Model exposing (..)
import Message exposing (..)
import View exposing (view)


main : Program Never Model Msg
main =
    Html.program { init = init, view = view, update = update, subscriptions = subscriptions }


init : ( Model, Cmd Msg )
init =
    { selection = None
    , pieces = []
    , boardSize = { x = 8, y = 8 }
    , winner = Nothing
    }
        ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectField index ->
            whenNotWon (fieldSelected index) model

        SelectChoice choice ->
            choiceSelected choice model

        WebSocketReceive data ->
            webSocketReceived data model

        ActionDone ->
            { model | selection = None } ! []


whenNotWon : (Model -> ( Model, Cmd Msg )) -> Model -> ( Model, Cmd Msg )
whenNotWon f model =
    case model.winner of
        Just color ->
            model ! []

        Nothing ->
            f model


choiceSelected : String -> Model -> ( Model, Cmd Msg )
choiceSelected choice model =
    case model.selection of
        SelectedNeedChoice selected index available ->
            List.filter (\( index, action ) -> (Maybe.withDefault "" action.choice) == choice) available
                |> List.map Tuple.first
                |> List.head
                |> Maybe.map (\i -> { model | selection = Selected selected index } ! [ Communication.execActionCmd (Position.fromIndex model.boardSize selected) i ])
                |> Maybe.withDefault ({ model | selection = None } ! [])

        _ ->
            { model | selection = None } ! []


webSocketReceived : String -> Model -> ( Model, Cmd Msg )
webSocketReceived data model =
    case (Communication.handleMessage data) of
        Update pieces winner x y ->
            { model | pieces = pieces, winner = winner, boardSize = { x = x, y = y } } ! [ Task.perform (\_ -> ActionDone) (Process.sleep (1 * second)) ]

        ValidActions validActions ->
            case model.selection of
                AvailablePending selected ->
                    { model | selection = Available selected validActions } ! []

                _ ->
                    { model | selection = None } ! []

        Invalid e ->
            model ! []


fieldSelected : Int -> Model -> ( Model, Cmd Msg )
fieldSelected index model =
    let
        ( selection, cmds ) =
            case model.selection of
                None ->
                    ( AvailablePending index, [ Communication.queryActionsCmd <| Position.fromIndex model.boardSize index ] )

                Available selected available ->
                    let
                        indexedActions =
                            List.indexedMap (,) available
                                |> List.filter (\t -> Position.toIndex model.boardSize (Tuple.second t).target == index)
                    in
                        if (List.length indexedActions) > 1 then
                            ( SelectedNeedChoice selected index indexedActions, [] )
                        else
                            indexedActions
                                |> List.map Tuple.first
                                |> List.head
                                |> Maybe.map (\i -> ( Selected selected index, [ Communication.execActionCmd (Position.fromIndex model.boardSize selected) i ] ))
                                |> Maybe.withDefault ( None, [] )

                _ ->
                    ( model.selection, [] )
    in
        { model | selection = selection } ! cmds


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen "ws://localhost:9000/socket" WebSocketReceive
