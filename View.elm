module View exposing (view)

import Html exposing (Html, div, text, img)
import Html.Attributes exposing (style, src)
import BoardView
import Model exposing (Piece)
import Position exposing (Size)
import Model exposing (..)
import Message exposing (..)


flexCenter =
    style
        [ ( "display", "flex" )
        , ( "justify-content", "center" )
        , ( "align-items", "center" )
        ]


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
                                |> List.map (Position.toIndex model.boardSize)
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
            , stylesheet
            , winnerDiv model.winner
            ]


stylesheet =
    Html.node
        "link"
        [ Html.Attributes.rel "stylesheet"
        , Html.Attributes.type_ "text/css"
        , Html.Attributes.href "assets/stylesheets/elm.css"
        ]
        []


winnerDiv maybeWinner =
    case maybeWinner of
        Just winner ->
            div
                [ style
                    [ ( "z-index", "4" )
                    , ( "font-size", "5em" )
                    , ( "background", "rgba(255, 64, 129, 0.9)" )
                    , ( "padding", "20px" )
                    ]
                ]
                [ text <| winner ++ " has won!" ]

        Nothing ->
            text ""
