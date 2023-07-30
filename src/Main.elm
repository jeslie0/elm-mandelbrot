port module Main exposing (..)

import Browser
import Color
import ComplexNumbers as C exposing (ComplexNumber(..))
import Html as H exposing (Html)
import Html.Attributes as HA
import Imaginary as I exposing (Imaginary(..))
import Process
import Real as R exposing (Real(..))
import Task



-- * PORTS


type alias MyColour =
    { red : Float
    , green : Float
    , blue : Float
    , alpha : Float
    }


type alias RowData =
    { row : Int
    , computedColours : List MyColour
    }


port sendInitialSettings : { height : Int, width : Int } -> Cmd msg


port settingsSet : (Int -> msg) -> Sub msg


port sendRow : RowData -> Cmd msg


port sendRows : List RowData -> Cmd msg



-- * MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = \flags -> init flags 800
        , view = \model -> { title = "", body = [ view model ] }
        , update = update
        , subscriptions = subscriptions
        }



-- * MODEL


type alias Model =
    { height : Int
    , width : Int
    , computedRow : Int
    , min : ComplexNumber Float
    , max : ComplexNumber Float
    }


init : flags -> Int -> ( Model, Cmd Msg )
init _ size =
    ( { width = size
      , height = size
      , computedRow = 0
      , min = ComplexNumber (Real -2) (Imaginary <| Real -1.5)
      , max = ComplexNumber (Real 1) (Imaginary <| Real 1.5)
      }
    , sendInitialSettings { width = size, height = size }
    )



-- * UPDATE


type Msg
    = CalculateNextRow
    | CalculateNextRows Int
    | SettingsSet


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SettingsSet ->
            ( model
            , Task.succeed ()
                |> Task.perform (\_ -> CalculateNextRows 10)
            )

        CalculateNextRow ->
            ( { model | computedRow = model.computedRow + 1 }
            , let
                computedRowData =
                    { row = model.computedRow
                    , computedColours = computeRow model.computedRow model
                    }
              in
              if model.computedRow > model.height then
                Cmd.none

              else
                Cmd.batch
                    [ sendRow computedRowData
                    , Process.sleep 0
                        |> Task.perform (\_ -> CalculateNextRow)
                    ]
            )

        CalculateNextRows n ->
            ( { model | computedRow = model.computedRow + n }
            , let
                computedRowsDataHelper row acc =
                    if row < 0 then
                        acc

                    else
                        computedRowsDataHelper (row - 1)
                            ({ row = model.computedRow + row
                             , computedColours = computeRow (model.computedRow + row) model
                             }
                                :: acc
                            )
              in
              if model.computedRow > model.height then
                Cmd.none

              else
                Cmd.batch
                    [ sendRows <| computedRowsDataHelper n []
                    , Process.sleep 0
                        |> Task.perform (\_ -> CalculateNextRows n)
                    ]
            )



-- * SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    settingsSet (\_ -> SettingsSet)



-- * VIEW


viewDocument : Model -> Browser.Document Msg
viewDocument model =
    { title = ""
    , body = [ view model ]
    }


view : Model -> Html Msg
view model =
    H.canvas [ HA.width model.width, HA.height model.height, HA.id "mandelbrot", HA.style "padding" "8px" ] []


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
normSquared z =
    R.real (R.multiply (C.real z) (C.real z) |> R.add (R.multiply (I.imaginary <| C.imaginary z) (I.imaginary <| C.imaginary z)))


determineColour : Int -> MyColour
determineColour iterations =
    let
        x =
            modBy 5 iterations
    in
    Color.toRgba <|
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


computeCell : Int -> Int -> Model -> MyColour
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
            determineColour value

        Nothing ->
            Color.toRgba Color.black


computeRow : Int -> Model -> List MyColour
computeRow row model =
    let
        helper : Int -> List MyColour -> List MyColour
        helper n acc =
            if 0 < n then
                helper (n - 1) (computeCell row n model :: acc)

            else
                acc
    in
    helper model.width []



-- L.foldl (\a b -> computeCell row a model :: b) [] (L.reverse <| L.range 0 model.width)
