module ViewHtml exposing (..)

import Mandelbrot
import Html as H exposing (Html)
import Html.Attributes as HA
import Dict exposing (Dict)
import Canvas
import Canvas.Settings as CanvasSettings
import Color exposing (Color)
import List as L

determineColor : Int -> String
determineColor iterations =
    let
        x =
            modBy 5 iterations
    in
        if x < 1 then
            "yellow"
        else if x < 2 then
            "orange"
        else if x < 3 then
            "red"
        else if x < 4 then
            "pink"
        else
            "white"

viewRow : Mandelbrot.Model -> Int -> Html msg
viewRow model row =
    H.div
         (L.map (\(key, val) -> HA.style key val) [("height", "1px")])
         (L.map (viewCell model row) <| L.range 0 model.width)


viewCell : Mandelbrot.Model -> Int -> Int -> Html msg
viewCell model row col =
    let
        colour =
            Dict.get ( col, row ) model.computed
            |> Maybe.map determineColour
            |> Maybe.withDefault (Color.toCssString Color.black)
    in
        H.div
            (L.map (\(key,val) -> HA.style key val)
                [ ( "width", "1px" )
                , ( "height", "1px" )
                , ( "background-color", colour )
                , ( "display", "inline-block" )
                ])
            []


determineColour : Int -> String
determineColour iterations =
    let x = modBy 5 iterations
    in Color.toCssString <|
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


view : Mandelbrot.Model -> Html msg
view model =
    H.div [ HA.style "padding" "8px" ]
        (L.map (viewRow model) <| L.range 0 model.height)
