module ViewCanvas exposing (view)

import Mandelbrot
import Html as H exposing (Html)
import Html.Attributes as HA
import Dict exposing (Dict)
import Canvas
import Canvas.Settings as CanvasSettings
import Color exposing (Color)
import List as L

view : Mandelbrot.Model -> Html msg
view model =
    H.div [ HA.style "padding" "8px" ]
        [ Canvas.toHtml ( model.width, model.height )
            []
            (L.concatMap (viewRow model) (L.range 0 model.height))
        ]


viewRow : Mandelbrot.Model -> Int -> List Canvas.Renderable
viewRow model row =
    L.map (viewCell model row) (L.range 0 model.width)


determineColour : Int -> Color
determineColour iterations =
    let x = modBy 5 iterations
    in
    if x < 1 then
        Color.yellow
    else if iterations < 2 then
        Color.orange
    else if iterations < 3 then
        Color.red
    else if iterations < 4 then
        Color.lightRed
    else
        Color.white


viewCell : Mandelbrot.Model -> Int -> Int -> Canvas.Renderable
viewCell model row col =
    let
        colour =
            Dict.get ( col, row ) model.computed
            |> Maybe.map determineColour
            |> Maybe.withDefault Color.black
    in
    Canvas.shapes [ CanvasSettings.fill colour ] [ Canvas.rect ( toFloat col, toFloat row ) 1 1 ]
