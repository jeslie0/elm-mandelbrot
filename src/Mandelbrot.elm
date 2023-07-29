module Mandelbrot exposing (Model, computeAll, computeRow, init, view)

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
    , min = ComplexNumber (Real -2) (Imaginary <| Real -2)
    , max = ComplexNumber (Real 2) (Imaginary <| Real 2)
    }


calculate : Int -> ComplexNumber Float -> Int -> ComplexNumber Float -> Maybe Int
calculate maxIterations c iterations z =
    let
        z_ =
            C.multiply z z |> C.add c
    in
    if iterations >= maxIterations then
        Nothing

    else if R.real (C.modulus z_) >= 2 then
        Just iterations

    else
        calculate maxIterations c (iterations + 1) z_


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
            calculate 200 c 0 c
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
        (L.map (viewRow model) (L.range 0 model.height))


viewRow : Model -> Int -> Html msg
viewRow model row =
    H.div [ HA.style "height" "2px" ]
        (L.map (viewCell model row) (L.range 0 model.width))


viewCell : Model -> Int -> Int -> Html msg
viewCell model row col =
    let
        colour =
            case Dict.get ( col, row ) model.computed of
                Just val ->
                    "yellow"

                Nothing ->
                    "black"
    in
    H.div
        (L.map (\( key, val ) -> HA.style key val)
            [ ( "width", "2px" )
            , ( "height", "2px" )
            , ( "background-color", colour )
            , ( "display", "inline-block" )
            ]
        )
        []
