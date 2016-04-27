import Html as H
import String exposing (join)
import Html.Attributes as HA
import Time exposing (every, second)
import Svg 
import List.Extra as LE exposing (andThen)
import Signal 
import Svg.Events exposing (onClick)
import Svg.Attributes exposing (version, viewBox, x, y, x1, y1, x2, y2, fill, style, width, height)

w = 450
h = 450
rowCount=12
colCount=12
dt = 0.03

type alias Cell = (Int, Int)

type alias Model = 
    { path : List Cell
    , board : List Cell
    }

init = 
    { path = []
    , board = [0..rowCount-1] `LE.andThen` \r ->
              [0..colCount-1] `LE.andThen` \c ->
              [(r, c)]
    }

center = HA.style [ ( "text-align", "center") ] 

view address model = 
    let
        showChecker row col = 
            Svg.rect [ x <| toString col
                     , y <| toString row 
                     , width "1"
                     , height "1"
                     , fill <| if (row + col) % 2 == 0 then "blue" else "grey"
                     , onClick <| Signal.message address <| SetStart (row, col)
                     ]
                     [] 

        showMove pt0 pt1 = 
            Svg.line [ x1 <| toString ((snd pt0 |> toFloat) + 0.5)
                     , y1 <| toString ((fst pt0 |> toFloat) + 0.5)
                     , x2 <| toString ((snd pt1 |> toFloat) + 0.5)
                     , y2 <| toString ((fst pt1 |> toFloat) + 0.5)
                     , style "stroke:yellow;stroke-width:0.05" 
                     ]
                     [] 

        checkers model = model.board `LE.andThen` \(r,c) ->
                         [showChecker r c]

        moves model = case List.tail model.path of
            Nothing -> []
            Just tl -> List.map2 showMove model.path tl

        unvisited = List.length model.board - List.length model.path

    in 
        H.div 
          []
          [ H.h2 [center] [H.text "Knight's Tour"]
          , H.h2 [center] [H.text <| "Unvisited count : " ++ toString unvisited ]
          , H.h2 [center] [H.text "(pick a square)"]
          , H.div 
              [center] 
              [ Svg.svg 
                  [ version "1.1"
                  , width (toString w)
                  , height (toString h)
                  , viewBox (join " " 
                               [ toString 0        
                               , toString 0        
                               , toString rowCount 
                               , toString colCount ])
                  ] 
                  [ Svg.g [] <| checkers model ++ moves model ]
              ]
          ] 

nextMoves : Model -> Cell -> List Cell
nextMoves model startCell = 
  let c = [ 1,  2, -1, -2]
      km = c `LE.andThen` \cx -> 
           c `LE.andThen` \cy -> 
           if abs(cx) == abs(cy) then [] else [(cx,cy)]
      jumps = List.map (\cell -> (fst cell + fst startCell, snd cell + snd startCell)) km
  in List.filter (\j -> List.member j model.board && not (List.member j model.path) ) jumps

bestMove : Model -> Maybe Cell
bestMove model = 
    case List.head (model.path) of
        Nothing -> Nothing
        Just mph -> LE.minimumBy (List.length << nextMoves model) (nextMoves model mph)

update action model =
    case action of
        SetStart start -> 
            {model |  path = [start]} 
        Tick t ->  
            if (model.path == []) then 
               model
           else
               case bestMove model of
                   Nothing -> model
                   Just nm -> {model | path = nm :: model.path }
        NoOp -> model

control = Signal.mailbox NoOp

type Action = NoOp | Tick Int | SetStart Cell

tickSignal = (every (dt * second)) |> Signal.map (\t -> Tick (round t)) 
actionSignal = Signal.mergeMany [tickSignal, control.signal]

modelSignal = Signal.foldp update init actionSignal

main = Signal.map (view control.address) modelSignal 
