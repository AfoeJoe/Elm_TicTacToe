module TicTacToe exposing (main)

import Array exposing (Array, get, initialize)
import Browser
import Html exposing (Html, button, div, li, text, ul)
import Html.Attributes exposing (class, list, value)
import Html.Events exposing (onClick)
import List exposing (head)


type alias Squares =
    Array Char


type alias History =
    Array { squares : Squares }


type alias Model =
    { history : History
    , stepNumber : Int
    , xIsNext : Bool
    }


type Msg
    = HandleClick Int
    | JumpTo Int
    | Restart



-- MODEL


initialModel : Model
initialModel =
    { history = initialize 1 (\_ -> { squares = Array.repeat 9 ' ' })
    , stepNumber = 0
    , xIsNext = True
    }



-- VIEW


view : Model -> Html Msg
view model =
    game model



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        HandleClick arg ->
            let
                history =
                    Array.slice 0 (model.stepNumber + 1) model.history

                sqaures =
                    get (Array.length history - 1) history

                filled =
                    case sqaures of
                        Just square ->
                            Maybe.withDefault ' ' (get arg square.squares) /= ' '

                        Nothing ->
                            False

                wonAlready =
                    calculateWinner (Maybe.withDefault { squares = Array.repeat 1 ' ' } sqaures).squares /= Nothing
            in
            if filled || wonAlready then
                model

            else
                case sqaures of
                    Just isSquares ->
                        let
                            char =
                                if model.xIsNext then
                                    'X'

                                else
                                    '0'

                            newSquareInHistory =
                                { squares = Array.set arg char isSquares.squares }
                        in
                        { model
                            | xIsNext = not model.xIsNext
                            , stepNumber = model.stepNumber + 1
                            , history = Array.append history (Array.fromList [ newSquareInHistory ])
                        }

                    Nothing ->
                        model

        JumpTo idx ->
            { model | stepNumber = idx, xIsNext = remainderBy 2 idx == 0 }

        Restart ->
            initialModel



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , update = update
        , view = view
        }



-- VIEW BUILDERS


game : Model -> Html Msg
game model =
    div [ class "game" ]
        [ div [ class "game-board" ]
            [ board (get model.stepNumber model.history) ]
        , div
            [ class "game-info" ]
            [ div [] [ text "Game Status" ]
            , gameInfo
                (calculateWinner
                    (Maybe.withDefault { squares = Array.repeat 0 ' ' } (get (Array.length model.history - 1) model.history)).squares
                )
                model.xIsNext
                model.stepNumber
            , div [] [ gameHistory model.history ]
            ]
        ]


board : Maybe { squares : Squares } -> Html Msg
board sqaures =
    case sqaures of
        Just realsqaures ->
            div [] <|
                (List.map
                    (\( a, b, c ) ->
                        div [ class "board-row" ]
                            [ renderSquare a (Maybe.withDefault ' ' (get a realsqaures.squares))
                            , renderSquare b (Maybe.withDefault ' ' (get b realsqaures.squares))
                            , renderSquare c (Maybe.withDefault ' ' (get c realsqaures.squares))
                            ]
                    )
                    << List.take 3
                )
                    lines

        Nothing ->
            div [] []


renderSquare : Int -> Char -> Html Msg
renderSquare a val =
    button [ class "square", onClick <| HandleClick a, value <| String.fromInt a ]
        [ text <| String.fromChar val ]


gameInfo : Maybe Char -> Bool -> Int -> Html Msg
gameInfo winner xIsNext noOfMoves =
    if noOfMoves == 9 then
        div [] [ text "Gameoover - No winner ", startAllOver ]

    else
        case winner of
            Just thereWasAWinner ->
                div [] [ text <| String.fromChar thereWasAWinner ++ " won", startAllOver ]

            Nothing ->
                div []
                    [ text <|
                        "Next Playyer is"
                            ++ (if xIsNext then
                                    "X"

                                else
                                    "0"
                               )
                    ]


gameHistory : History -> Html Msg
gameHistory history =
    div []
        [ div [] [ text <| "Move History" ]
        , ul [] <|
            (Array.toList
                << Array.indexedMap
                    (\idx _ ->
                        let
                            move =
                                if idx == 0 then
                                    "Go To start"

                                else
                                    "Go To Move" ++ String.fromInt idx
                        in
                        li []
                            [ button [ onClick (JumpTo idx) ] [ text move ]
                            ]
                    )
            )
                history
        ]


startAllOver : Html Msg
startAllOver =
    div [] [ button [ onClick Restart ] [ text "start Over" ] ]



-- UTILS


calculateWinner : Squares -> Maybe Char
calculateWinner arg =
    let
        recurse list =
            let
                ( a, b, c ) =
                    Maybe.withDefault ( 0, 0, 0 ) (head list)

                aValue =
                    Maybe.withDefault ' ' (get a arg)

                bValue =
                    Maybe.withDefault ' ' (get b arg)

                cValue =
                    Maybe.withDefault ' ' (get c arg)
            in
            if List.isEmpty list || b == 0 then
                Nothing

            else if aValue /= ' ' && aValue == bValue && aValue == cValue then
                Just aValue

            else
                recurse (Maybe.withDefault [] (List.tail list))
    in
    recurse lines



-- CONSTANTS


lines : List ( Int, Int, Int )
lines =
    [ ( 0, 1, 2 )
    , ( 3, 4, 5 )
    , ( 6, 7, 8 )
    , ( 0, 3, 6 )
    , ( 1, 4, 7 )
    , ( 2, 5, 8 )
    , ( 0, 4, 8 )
    , ( 2, 4, 6 )
    ]
