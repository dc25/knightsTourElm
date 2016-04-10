import Html exposing (..)
import String exposing (join)
import Html.Attributes as HA
import Time exposing (every, second)
import Svg 
import Signal exposing (..)
import Svg.Events exposing (onClick)
import Svg.Attributes exposing (version, viewBox, cx, cy, r, x, y, x1, y1, x2, y2, fill,points, style, width, height, preserveAspectRatio)

w = 500
h = 500
dt = 0.5

-- Taken verbatim from comments in : https://github.com/elm-lang/elm-compiler/issues/147
andMap : List (a -> b) -> List a -> List b -- like (<*>) in Haskell, specialised to lists
andMap listOfFuncs listOfAs = List.concatMap (\f -> List.map f listOfAs) listOfFuncs

type alias Cell = (Int, Int)

type alias Model = 
    { rows : Int
    , cols : Int
    , path : List Cell
    , board : List Cell
    }

model rc cc = 
        { rows=rc
        , cols=cc
        , path = []
        , board = List.map (\r c -> {row=r, col=c}) [0..rc-1] `andMap` [0..cc-1]
        }

init = model

floatLeft = HA.style [ ("float", "left") ] 
centerTitle = HA.style [ ( "text-align", "center") ] 

view address model = 
    let
        showChecker row col = 
            Svg.rect [ x <| toString col, 
                       y <| toString row, 
                       width "1", 
                       height "1", 
                       fill <| if (row + col) % 2 == 0 then "blue" else "grey", 
                       onClick <| message address <| SetStart (row, col)
                     ]
                     [] 

        checkers rows cols = List.concatMap (\at -> List.map (showChecker at) [0..cols-1]) [0..rows-1]

    in 
        div 
          []
          [ h2 [centerTitle] [text "Knights Tour"]
          , div 
              [centerTitle] 
              [ Svg.svg 
                  [ version "1.1"
                  , width (toString w)
                  , height (toString h)
                  , viewBox (join " " 
                               [ 0          |> toString
                               , 0          |> toString
                               , model.cols |> toString
                               , model.rows |> toString ])
                  ] 
                  [ Svg.g [] <| checkers model.rows model.cols]
              ]
          ] 

knightMoves : Model -> Cell -> List Cell
knightMoves model startCell = 
  let km    = [ ( 1,  2)
              , ( 1, -2)
              , (-1,  2)
              , (-1, -2)
              , ( 2,  1)
              , ( 2, -1)
              , (-2,  1)
              , (-2, -1) 
              ]
      jumps = List.map (\cell -> (fst cell + fst startCell, snd cell + snd startCell)) km
  in List.filter (flip List.member model.board) jumps

nextMove : Model -> Maybe Cell
nextMove model = 
    let 
        findMoves sq = List.filter (\mv -> not <| (flip List.member) (model.path) mv) (knightMoves model sq) 
        candMoves = case List.head (model.path) of
            Nothing -> []
            Just mph -> findMoves mph
        nextMovesLength = List.map (\mv -> (findMoves mv |> List.length, mv)) candMoves
        bestMove = List.minimum nextMovesLength 
    in 
       case bestMove of
           Nothing -> Nothing
           Just nm -> Just (snd nm)

update action model =
    case action of
        SetStart start -> 
            {model |  path = [start]} 
        Tick t ->  
            if (model.path == []) then 
               model
           else
               model
        NoOp -> model

control = Signal.mailbox NoOp

type Action = NoOp | Tick Int | SetStart Cell

tickSignal = (every (dt * second)) |> Signal.map (\t -> Tick (round t)) 

actionSignal = Signal.mergeMany [tickSignal, control.signal]

modelSignal = Signal.foldp update (init 8 8 ) actionSignal

main = Signal.map (view control.address) modelSignal 
