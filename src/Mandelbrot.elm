module Mandelbrot exposing (Model, computeAll, computeRow, init, view)

import Canvas
import Canvas.Settings as CanvasSettings
import Color exposing (Color)
import ComplexNumbers as C exposing (ComplexNumber(..))
import Dict exposing (Dict)
import Html as H exposing (Html)
import Html.Attributes as HA
import Imaginary as I exposing (Imaginary(..))
import List as L
import Real as R exposing (Real(..))


type alias Point =
    ( Int, Int )


type alias Model =
    { width : Int
    , height : Int
    , computed : Dict Point Int
    , min : ComplexNumber Float
    , max : ComplexNumber Float
    }


init : Int -> Model
init size =
    { width = size
    , height = size
    , computed = Dict.empty
    , min = ComplexNumber (Real -2) (Imaginary <| Real -1.5)
    , max = ComplexNumber (Real 1) (Imaginary <| Real 1.5)
    }


calculate : Int -> ComplexNumber Float -> Int -> ComplexNumber Float -> Maybe Int
calculate maxIterations c iterations z =
    let
        z_ =
            C.multiply z z |> C.add c
    in
    if iterations >= maxIterations then
        Nothing

    else if normSquared z_ >= 4 then
        Just iterations

    else
        calculate maxIterations c (iterations + 1) z_

normSquared : ComplexNumber number -> number
normSquared z = R.real (R.multiply (C.real z) (C.real z) |> R.add (R.multiply (I.imaginary <| C.imaginary z) (I.imaginary <| C.imaginary z)))

computeCell : Int -> Int -> Model -> Model
computeCell row col model =
    let
        colPercent =
            toFloat col / toFloat model.width

        rowPercent =
            toFloat row / toFloat model.height

        cRe =
            C.real model.min |> R.add ((C.real model.max |> R.add (R.negate <| C.real model.min)) |> R.multiply (Real colPercent))

        cIm =
            (I.imaginary <| C.imaginary <| model.min) |> R.add (((I.imaginary <| C.imaginary model.max) |> R.add (R.negate <| I.imaginary <| C.imaginary model.min)) |> R.multiply (Real rowPercent))

        c =
            ComplexNumber
                cRe
                (Imaginary cIm)

        valueM =
            calculate 100 c 0 c
    in
    case valueM of
        Just value ->
            { model | computed = Dict.insert ( col, row ) value model.computed }

        Nothing ->
            { model | computed = Dict.remove ( col, row ) model.computed }


computeRow : Int -> Model -> Model
computeRow row model =
    L.foldl (computeCell row) model <| L.range 0 model.width


computeAll : Model -> Model
computeAll model =
    L.foldl computeRow model <| L.range 0 model.height


view : Model -> Html msg
view model =
    H.div [ HA.style "padding" "8px" ]
        [ Canvas.toHtml ( model.width, model.height )
            []
            (L.concatMap (viewRow model) (L.range 0 model.height))
        ]


viewRow : Model -> Int -> List Canvas.Renderable
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


viewCell : Model -> Int -> Int -> Canvas.Renderable
viewCell model row col =
    let
        colour =
            Dict.get ( col, row ) model.computed
            |> Maybe.map determineColour
            |> Maybe.withDefault Color.black
    in
    Canvas.shapes [ CanvasSettings.fill colour ] [ Canvas.rect ( toFloat col, toFloat row ) 1 1 ]



-- H.div
--     (L.map (\( key, val ) -> HA.style key val)
--         [ ( "width", "2px" )
--         , ( "height", "2px" )
--         , ( "background-color", colour )
--         , ( "display", "inline-block" )
--         ]
--     )
--     []
