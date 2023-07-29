module Mandelbrot exposing (Model, init, view)

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
    }


init : Int -> Model
init size =
    { width = size
    , height = size
    , computed = Dict.empty |> Dict.insert ( 5, 5 ) 1
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


computeCell : Point -> Model -> Model
computeCell ( col, row ) model =
    let
        c =
            ComplexNumber
                (Real <| 2 * toFloat col / toFloat model.width)
                (Imaginary << Real <| 2 * toFloat row / toFloat model.height )

        valueM =
            calculate 100 C.zero 0 c
    in
    case valueM of
        Just value ->
            { model | computed = Dict.insert ( col, row ) value model.computed }

        Nothing ->
            { model | computed = Dict.remove ( col, row ) model.computed }


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
                    "yello"

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
