module BoardView exposing (draw, BoardModel)

import Html exposing (Html, div, text, img)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style, src, class)
import Model exposing (..)
import Position exposing (Size)
import Html.Keyed
import Action exposing (Action)
import Message exposing (..)


type alias BoardModel a =
    { boardSize : Size
    , selected : Int
    , selectedTarget : Int
    , targets : List Int
    , pieces : List Piece
    , choices : List String
    , onClick : Int -> a
    }


type Side
    = Left
    | Right


draw : BoardModel Msg -> Html Msg
draw model =
    div
        [ style
            [ ( "width", toString (80 / (toFloat model.boardSize.y / toFloat model.boardSize.x)) ++ "vh" )
            , ( "height", "80vh" )
            , ( "max-width", "80vw" )
            , ( "position", "absolute" )
            , ( "flex-wrap", "wrap" )
            , ( "align-items", "center" )
            , ( "display", "flex" )
            , ( "align-content", "center" )
            , ( "justify-content", "center" )
            ]
        ]
        (List.range 0 ((model.boardSize.x * model.boardSize.y) - 1)
            |> List.map
                (\i ->
                    square
                        model.boardSize
                        (model.onClick i)
                        (backgroundColor model.boardSize.x i)
                        (highliteColor i model.selected model.selectedTarget model.targets)
                    <|
                        (if i == 0 then
                            List.map (\p -> ( (toString p.id), pieceDiv p )) model.pieces
                         else
                            []
                        )
                            ++ (if i == model.selected then
                                    [ ( "selection", selectionDiv model.choices Right ) ]
                                else
                                    []
                               )
                )
        )


getImg : Int -> List { c | img : String, pos : Int } -> Maybe String
getImg index pieces =
    List.filter (\p -> p.pos == index) pieces
        |> List.head
        |> Maybe.map .img



-- https://www.materialpalette.com/indigo/pink


highliteColor : Int -> Int -> Int -> List Int -> String
highliteColor index selected selectedTarget targets =
    if index == selectedTarget then
        "linear-gradient(45deg, rgba(68, 138, 255,0.7), rgba(68, 138, 255,0.7))"
    else if List.any (\i -> index == i) targets then
        "linear-gradient(45deg, rgba(48, 63, 159,0.7), rgba(48, 63, 159,0.7))"
    else if index == selected then
        if selectedTarget /= -1 then
            "linear-gradient(45deg, rgba(255, 64, 129, 0.7), rgba(255, 64, 129, 0.7))"
        else
            "linear-gradient(45deg, rgba(233, 30, 99, 0.7), rgba(233, 30, 99, 0.7))"
    else
        "none"


backgroundColor : Int -> Int -> String
backgroundColor sizeX index =
    let
        lineWrapping =
            (index // sizeX) * ((sizeX % 2) + 1)
    in
        if (index % 2 + lineWrapping) % 2 == 0 then
            "#838383"
        else
            "#F1F1F1"


square boardSize msg background highlite =
    let
        padding =
            boardSize.x
                |> toFloat
                |> (*) 2
                |> (/) 100
                |> toString
                |> flip (++) "%"
    in
        Html.Keyed.node "div"
            [ style
                [ ( "padding", padding )
                , ( "position", "relative" )
                , ( "background", background )
                , ( "background-image", highlite )
                , ( "box-shadow", "30px 30px 5px rgba(80,80,80,0.8)" )
                ]
            , onClick (msg)
            ]


pieceDiv : Piece -> Html msg
pieceDiv piece =
    let
        asPercentString x =
            (toString x) ++ "%"

        transform =
            "translate("
                ++ (asPercentString (piece.pos.x * 100))
                ++ ", "
                ++ (asPercentString (piece.pos.y * 50))
                ++ ")"
    in
        div
            [ style
                [ ( "width", "100%" )
                , ( "height", "200%" )
                , ( "position", "absolute" )
                , ( "left", "0" )
                , ( "bottom", "0" )
                , ( "pointer-events", "none" )
                , ( "display", "flex" )
                , ( "justify-content", "center" )
                , ( "transform", transform )
                , ( "z-index", "1" )
                , ( "transition-duration", "1s" )
                , ( "transition-property", "transform" )
                , ( "transition-timing-function", "ease " )
                ]
            ]
            [ img
                [ src piece.img
                , style
                    [ ( "max-width", "80%" )
                    , ( "max-height", "100%" )
                    , ( "position", "absolute" )
                    , ( "bottom", "5%" )
                    , ( "z-index", "1" )
                    ]
                ]
                []
            ]


selectionDiv : List String -> Side -> Html Msg
selectionDiv items side =
    let
        show =
            True

        -- (List.length items) > 1
        width =
            case show of
                True ->
                    (toString <| List.length items)
                        ++ "00%"

                False ->
                    "0%"
    in
        div
            [ style
                [ ( "height", "100%" )
                , ( "width", width )
                , ( "position", "absolute" )
                , ( "overflow", "hidden" )
                , ( "bottom", "16px" )
                , ( "right", "calc(" ++ "-" ++ width ++ " + 16px)" )
                , ( "z-index", "2" )
                , ( "display", "flex" )
                , ( "transition-property", "width right left" )
                , ( "background", "#BDBDBD" )
                , ( "box-sizing", "border-box" )
                , ( "transition-duration", "250ms" )
                , ( "justify-content", "space-around" )
                , ( "align-items", "center" )
                , ( "class", "selection-outer" )
                , ( "box-shadow", "rgba(117, 117, 117, 0.7) 16px 16px" )
                ]
            ]
            (List.map mapChoiceToImgURL items
                |> List.map selectionImage
            )


mapChoiceToImgURL : String -> ( String, String )
mapChoiceToImgURL choice =
    ( choice, "assets/pieces/" ++ "Black_" ++ choice ++ ".png" )



-- http://dabblet.com/gist/1498446


selectionImage selectionItem =
    img
        [ src (Tuple.second selectionItem)
        , style
            [ ( "height", "100%" )
            , ( "width", "auto" )
            , ( "display", "block" )
            , ( "max-height", "100%" )
            ]
        , class "selection"
        , onClick (SelectChoice (Tuple.first selectionItem))
        ]
        []
