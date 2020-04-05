module Board exposing (..)

import Dict exposing (Dict)
import Flip exposing (flip)
import List.Extra exposing (find, takeWhile)
import Maybe.Extra
import Debug exposing (log)



type alias Board =
    { pieces : Dict (Int, Int) Piece
    , selected : Maybe (Int, Int)
    }

type alias Piece =
    { color : Color
    , kind : Kind
    }

type Color = White | Black

type Kind = King | Queen | Rook | Knight | Bishop | Pawn

initBoard : Board
initBoard = { pieces = Dict.fromList
    [ ((0, 0), { color = White, kind = Rook })
    , ((1, 0), { color = White, kind = Knight })
    , ((2, 0), { color = White, kind = Bishop })
    , ((3, 0), { color = White, kind = Queen })
    , ((4, 0), { color = White, kind = King })
    , ((5, 0), { color = White, kind = Bishop })
    , ((6, 0), { color = White, kind = Knight })
    , ((7, 0), { color = White, kind = Rook })

    , ((0, 1), { color = White, kind = Pawn })
    , ((1, 1), { color = White, kind = Pawn })
    , ((2, 1), { color = White, kind = Pawn })
    , ((3, 1), { color = White, kind = Pawn })
    , ((4, 1), { color = White, kind = Pawn })
    , ((5, 1), { color = White, kind = Pawn })
    , ((6, 1), { color = White, kind = Pawn })
    , ((7, 1), { color = White, kind = Pawn })

    , ((0, 6), { color = Black, kind = Pawn })
    , ((1, 6), { color = Black, kind = Pawn })
    , ((2, 6), { color = Black, kind = Pawn })
    , ((3, 6), { color = Black, kind = Pawn })
    , ((4, 6), { color = Black, kind = Pawn })
    , ((5, 6), { color = Black, kind = Pawn })
    , ((6, 6), { color = Black, kind = Pawn })
    , ((7, 6), { color = Black, kind = Pawn })

    , ((0, 7), { color = Black, kind = Rook })
    , ((1, 7), { color = Black, kind = Knight })
    , ((2, 7), { color = Black, kind = Bishop })
    , ((3, 7), { color = Black, kind = Queen })
    , ((4, 7), { color = Black, kind = King })
    , ((5, 7), { color = Black, kind = Bishop })
    , ((6, 7), { color = Black, kind = Knight })
    , ((7, 7), { color = Black, kind = Rook })

    ]
    , selected = Maybe.Just (1,0)}

nextMoves : Board -> (Int, Int) -> List (Int, Int)
nextMoves board (x, y) =
    let
        pieceMaybe = Dict.get (x, y) board.pieces
    in
        case pieceMaybe of
            Maybe.Nothing -> []
            Maybe.Just piece ->
                case piece.kind of
                    King -> nextKingMoves board piece.color (x, y)
                    Queen -> nextQueenMoves board piece.color (x, y)
                    Rook -> nextRookMoves board piece.color (x, y)
                    Bishop -> nextBishopMoves board piece.color (x, y)
                    Knight -> nextKnightMoves board piece.color (x, y)
                    Pawn -> nextPawnMoves board piece.color (x, y)

nextKingMoves : Board -> Color -> (Int, Int) -> List (Int, Int)
nextKingMoves board color (x, y) =
    allDirections
        |> List.concatMap (nextMovesInDir 1 board { color = color, kind = King } (x, y))

nextQueenMoves : Board -> Color -> (Int, Int) -> List (Int, Int)
nextQueenMoves board color (x, y) =
    allDirections
        |> List.concatMap (nextMovesInDir 8 board { color = color, kind = King } (x, y))


nextRookMoves : Board -> Color -> (Int, Int) -> List (Int, Int)
nextRookMoves board color (x, y) =
    axialDirections 
        |> List.concatMap (nextMovesInDir 8 board { color = color, kind = King } (x, y))


nextBishopMoves : Board -> Color -> (Int, Int) -> List (Int, Int)
nextBishopMoves board color (x, y) =
    diagonalDirections 
        |> List.concatMap (nextMovesInDir 8 board { color = color, kind = King } (x, y))

nextKnightMoves : Board -> Color -> (Int, Int) -> List (Int, Int)
nextKnightMoves board color (x, y) =
    knightDirections 
        |> List.concatMap (nextMovesInDir 1 board { color = color, kind = King } (x, y))

nextPawnMoves : Board -> Color -> (Int, Int) -> List (Int, Int)
nextPawnMoves board color (x, y) =
    let
        dy =
            case color of
                White -> 1
                Black -> -1
        straightMoves =
            (if y == 1 || y == 6
                then [ (x, y+dy), (x, y+2*dy)]
                else [ (x, y+dy) ])
            |> List.filter isInBoard
            |> List.filter (\p -> not (Dict.member p board.pieces))
                    
        crossMoves =
            [(x+1, y+dy), (x-1, y+dy)]
                |> List.filter (
                    \pos -> 
                        Dict.get pos board.pieces
                            |> Maybe.map .color
                            |> Maybe.Extra.filter ((/=) color)
                            |> Maybe.Extra.isJust)
    in
        straightMoves ++ crossMoves
    

{-|
    Gives the list of accessible positions on the board when moving in the
    specific direction. The piece of opposite color that can be attacked is also
    included.
-}
nextMovesInDir : Int -> Board -> Piece -> (Int, Int) -> (Int, Int) -> List (Int, Int)
nextMovesInDir scale board piece (x, y) (dx, dy) =
    let
        nexts = 
            List.range 1 scale
                |> List.map (\i -> (dx*i, dy*i))
                |> List.map (\(deltaX, deltaY) -> (x+deltaX, y+deltaY))
                |> takeWhile isInBoard
        nearestFilledPos =
            find (flip Dict.member board.pieces) nexts
        nearestPiece = Maybe.andThen (flip Dict.get board.pieces) nearestFilledPos
    in
    case (nearestFilledPos, nearestPiece) of
        (Maybe.Just (fx, fy), Maybe.Just cPiece) ->
            let
                unoccluded = takeWhile ((/=) (fx, fy)) nexts
            in
                if cPiece.color == piece.color then
                    unoccluded
                else
                    unoccluded ++ [(fx, fy)]
        _ -> nexts


    

allDirections : List (Int, Int)
allDirections =
    [ (1, 0), (1, 1), (0, 1), (-1, 1)
    , (-1, 0), (-1, -1), (0, -1), (1, -1)]

diagonalDirections : List (Int, Int)
diagonalDirections = [ (1, 1), (-1, 1), (-1, -1), (1, -1)]

axialDirections : List (Int, Int)
axialDirections = [ (1, 0), (0, 1), (-1, 0), (0, -1)]

knightDirections : List (Int, Int)
knightDirections =
    [ (2, 1), (1, 2), (-1, 2), (-2, 1)
    , (-2, -1), (-1, -2), (1, -2), (2, -1)]

isInBoard : (Int, Int) -> Bool
isInBoard (x, y) =
    (x >= 0) && (x <= 7) && (y >= 0) && (y <= 7)


makeMove : Board -> (Int, Int) -> (Int, Int) -> Board
makeMove board (fx, fy) (tx, ty) =
    case Dict.get (fx, fy) board.pieces of
        Maybe.Nothing -> board
        Maybe.Just piece
            -> 
                { pieces =
                    board.pieces
                        |> Dict.remove (fx, fy)
                        |> Dict.insert (tx, ty) piece
                , selected = Maybe.Nothing}


pieceSvgPath : Piece -> String
pieceSvgPath piece =
    "resources/pieces/" ++
    (case piece.color of
        White -> "w"
        Black -> "b") ++
    (case piece.kind of
        King -> "k"
        Queen -> "q"
        Rook -> "r"
        Knight -> "n"
        Bishop -> "b"
        Pawn -> "p") ++ ".svg"
            
    