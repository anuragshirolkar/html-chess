module Main exposing (..)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Html exposing (h1)
import Html exposing (table)
import Html exposing (tr)
import Html exposing (td)
import Html.Attributes exposing (class)
import Board exposing (..)
import Dict
import Debug exposing (log)
import Set exposing (Set)
import Browser
import Maybe.Extra
import Html exposing (img)
import Html.Attributes exposing (src)



-- MAIN


main : Program () Model Msg
main = Browser.sandbox {
    init = init,
    update = update,
    view = view }



-- MODEL

type alias Model = 
    { board : Board
    , turn : Color }

init : Model
init =
  { board = initBoard
  , turn = White
  }


-- UPDATE

type Msg =
    CellClick Int Int

update : Msg -> Model -> Model
update msg model =
    case msg of
        CellClick x y -> handleCellClick model x y


handleCellClick : Model -> Int -> Int -> Model
handleCellClick model x y =
    let board = model.board in
    case board.selected of
        -- if no piece is selected, select a piece of your color
        Maybe.Nothing ->
            case Dict.get (x, y) board.pieces of
                Maybe.Nothing ->
                    model
            
                Maybe.Just piece ->
                    if piece.color == model.turn
                    then { model | board = { board | selected = Maybe.Just (x, y) }}
                    else model
    
        Maybe.Just selected ->
            let
                accessible = Set.fromList (nextMoves model.board selected)
            in
                if Set.member (x, y) accessible then
                    { model | board = makeMove board selected (x, y), turn = changeTurn model.turn}
                else if selected == (x, y) then
                    { model | board = { board | selected = Maybe.Nothing }}
                else if Dict.member (x, y) board.pieces then
                    { model | board = { board | selected = Maybe.Just (x, y) }}
                else
                    model
            
changeTurn : Color -> Color
changeTurn color =
    case color of
        White -> Black
        Black -> White
    

-- VIEW

view : Model -> Html Msg
view model =
    let
        accessible =
            case model.board.selected of
                Maybe.Nothing -> Set.empty
                Maybe.Just selected -> Set.fromList (nextMoves model.board selected)
    in
        table [ class "main-container"]
        [ tr [] [ td [ class "board-container" ]
          [List.range 0 7
            |> List.reverse
            |> List.map (renderRow model.board accessible)
            |> (::) renderHeader
            |> table []]
        , td [ class "panel" ]
          [ text ((if model.turn == White then "White" else "Black") ++ " plays") ]]]

renderHeader : Html Msg
renderHeader =
    tr []
    [ td [] []
    , td [class "header-cell"] [text "a"]
    , td [class "header-cell"] [text "b"]
    , td [class "header-cell"] [text "c"]
    , td [class "header-cell"] [text "d"]
    , td [class "header-cell"] [text "e"]
    , td [class "header-cell"] [text "f"]
    , td [class "header-cell"] [text "g"]
    , td [class "header-cell"] [text "h"]
    ]
    

renderRow : Board.Board -> Set (Int, Int) -> Int -> Html Msg
renderRow board accessible y =
    List.range 0 7
        |> List.map (\x -> renderCell board (Set.member (x, y) accessible) y x)
        |> (::) (td [class "index-cell"] [text (String.fromInt (y+1))])
        |> tr []

renderCell : Board.Board -> Bool -> Int -> Int -> Html Msg
renderCell board isAccessible y x =
    let
        value =
            Dict.get (x, y) board.pieces
                |> Maybe.map pieceSvgPath
                |> Maybe.map (\path -> img [ src path] [])
                |> Maybe.Extra.toList
        isWhite = modBy 2 (x+y) == 0
        cellClass = 
            if isAccessible && isWhite then "accessible-white-cell"
            else if isAccessible then "accessible-black-cell"
            else if board.selected == Maybe.Just (x, y) then "selected-cell"
            else if isWhite then "white-cell" 
            else "black-cell"
    in
    td [ class (cellClass ++ " cell"), onClick (CellClick x y)] value

pieceText : Piece -> String
pieceText piece =
    let
        kindText =
            case piece.kind of
                King -> "king"
                Queen -> "queen"
                Rook -> "rook"
                Knight -> "knight"
                Bishop -> "bishop"
                Pawn -> "pawn"
        colorText =
            case piece.color of
                White -> "w"
                Black -> "b"
    in
        colorText ++ " " ++ kindText
    
            
    