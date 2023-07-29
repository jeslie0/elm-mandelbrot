module Mandelbrot exposing (Model, init, view)

import Dict exposing (Dict)
import Html as H exposing (Html)
import Html.Attributes as HA
import List as L


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
    , computed = Dict.empty |> Dict.insert (5,5) 1
    }


view : Model -> Html msg
view model =
    H.div [ HA.style "padding" "8px"]
        (L.map (viewRow model) (L.range 0 model.height))


viewRow : Model -> Int -> Html msg
viewRow model row =
    H.div [HA.style "height" "2px"]
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
            , ("display", "inline-block")]
        )
        []
