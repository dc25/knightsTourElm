import Html exposing (..)
import String exposing (join)
import Html.Attributes as HA
import Svg 
import Svg.Attributes exposing (version, viewBox, cx, cy, r, x, y, x1, y1, x2, y2, fill,points, style, width, height, preserveAspectRatio)

w = 500
h = 500

type alias Model = 
    { rows : Int
    , cols : Int
    }

model = {rows=8,cols=8}

floatLeft = HA.style [ ("float", "left") ] 
centerTitle = HA.style [ ( "text-align", "center") ] 

main = 
    let
        showChecker row col = 
            Svg.rect [ x <| toString col, 
                       y <| toString row, 
                       width "1", 
                       height "1", 
                       fill <| if (row + col) % 2 == 0 then "blue" else "grey"] 
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
