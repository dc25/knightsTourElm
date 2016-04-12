import Html exposing (..)
import String exposing (join)
import Html.Attributes as HA
import Time exposing (every, second)
import Svg 
import List.Extra as LE exposing (andThen)
import Signal exposing (..)
import Svg.Events exposing (onClick)
import Svg.Attributes exposing (version, viewBox, x, y, x1, y1, x2, y2, fill, style, width, height)

w = 500
h = 500
dt = 0.01

type alias Cell = (Int, Int)

type alias Model = 
    { rows : Int
    , cols : Int
    , path : List Cell
    , board : List Cell
    }

init rc cc = 
    { rows=rc
    , cols=cc
    , path = []
    , board = [0..rc-1] `LE.andThen` \r ->
              [0..cc-1] `LE.andThen` \c ->
              [(r, c)]
    }

floatLeft = HA.style [ ("float", "left") ] 
centerTitle = HA.style [ ( "text-align", "center") ] 

view address model = 
    let
        showChecker row col = 
            Svg.rect [ x <| toString col
                     , y <| toString row 
                     , width "1"
                     , height "1"
                     , fill <| if (row + col) % 2 == 0 then "blue" else "grey"
                     , onClick <| message address <| SetStart (row, col)
                     ]
                     [] 

        showMove pt0 pt1 = 
            Svg.line [ x1 <| toString ((snd pt0 |> toFloat) + 0.5)
                     , y1 <| toString ((fst pt0 |> toFloat) + 0.5)
                     , x2 <| toString ((snd pt1 |> toFloat) + 0.5)
                     , y2 <| toString ((fst pt1 |> toFloat) + 0.5)
                     , style "stroke:black;stroke-width:0.05" 
                     ]
                     [] 

        checkers rows cols = [0..rows-1] `LE.andThen` \r ->
                             [0..cols-1] `LE.andThen` \c ->
                             [showChecker r c]

        moves pts = case List.tail pts of
            Nothing -> []
            Just tl -> List.map2 showMove pts tl

    in 
        div 
          []
          [ h2 [centerTitle] [text "Knights Tour"]
          , h2 [centerTitle] [text (toString (List.length model.path))]
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
                  [ Svg.g [] <| checkers model.rows model.cols ++ moves model.path]
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
  in List.filter (\j -> List.member j model.board && not (List.member j model.path) ) jumps

nextMove : Model -> Maybe Cell
nextMove model = 
    case List.head (model.path) of
        Nothing -> Nothing
        Just mph -> LE.minimumBy (List.length << knightMoves model) (knightMoves model mph)

update action model =
    case action of
        SetStart start -> 
            {model |  path = [start]} 
        Tick t ->  
            if (model.path == []) then 
               model
           else
               case nextMove model of
                   Nothing -> model
                   Just nm -> {model | path = nm :: model.path }
        NoOp -> model

control = Signal.mailbox NoOp

type Action = NoOp | Tick Int | SetStart Cell

tickSignal = (every (dt * second)) |> Signal.map (\t -> Tick (round t)) 

actionSignal = Signal.mergeMany [tickSignal, control.signal]

modelSignal = Signal.foldp update (init 8 8) actionSignal

main = Signal.map (view control.address) modelSignal 
