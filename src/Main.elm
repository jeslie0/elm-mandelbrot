port module Main exposing (..)

import Browser
import Color
import ColourMaps.Turbo exposing (turboColourMap)
import ColourMaps.Viridis exposing (viridisColourMapRefined)
import Complex as C exposing (Complex)
import Html as H exposing (Html)
import Html.Attributes as HA
import Maybe
import Process
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


type alias Settings =
    { height : Int
    , width : Int
    , canvasId : String
    }


port sendInitialSettings : Settings -> Cmd msg


port settingsSet : ({} -> msg) -> Sub msg


port sendRow : RowData -> Cmd msg


port sendRows : List RowData -> Cmd msg



-- * MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = \flags -> init flags 1080
        , view = \model -> { title = "", body = [ view model ] }
        , update = update
        , subscriptions = subscriptions
        }



-- * MODEL


type alias Model =
    { height : Int
    , width : Int
    , computedRow : Int
    , min : Complex
    , max : Complex
    , canvasId : String
    , maxIterations : Int
    , batchSize : Int
    }


init : flags -> Int -> ( Model, Cmd Msg )
init _ size =
    let
        canvasId =
            "mandelbrot"
    in
    ( { width = size
      , height = size
      , computedRow = 0
      , min = C.complex -2 -1.5
      , max = C.complex 1 1.5
      , canvasId = canvasId
      , maxIterations = 40
      , batchSize = 50
      }
    , sendInitialSettings { width = size, height = size, canvasId = canvasId }
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
                |> Task.perform (\_ -> CalculateNextRows model.batchSize)
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
    { title = "Mandelbrot"
    , body = [ view model ]
    }


view : Model -> Html Msg
view model =
    H.canvas [ HA.width model.width, HA.height model.height, HA.id model.canvasId ] []


calculate : Int -> Complex -> Int -> Complex -> Maybe Float
calculate maxIterations c iterations z =
    let
        z_ =
            C.multiply z z |> C.add c
    in
    if iterations >= maxIterations then
        Nothing

    else if normSquared z_ >= 4 then
        let
            nSmooth =
                toFloat iterations + 1 - logBase e (logBase e (C.toPolar z_).abs) / logBase e 2
        in
        Just <| (nSmooth / toFloat maxIterations)

    else
        calculate maxIterations c (iterations + 1) z_


normSquared : Complex -> Float
normSquared z =
    let
        zCartesian =
            C.toCartesian z

        zRe =
            zCartesian.re

        zIm =
            zCartesian.im
    in
    (zRe * zRe) + (zIm * zIm)


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

        minCartesian =
            C.toCartesian model.min

        maxCartesian =
            C.toCartesian model.max

        minRe =
            minCartesian.re

        minIm =
            minCartesian.im

        maxRe =
            maxCartesian.re

        maxIm =
            maxCartesian.im

        cRe =
            minRe + (maxRe - minRe) * colPercent

        cIm =
            minIm + (maxIm - minIm) * rowPercent

        c =
            C.complex cRe cIm

        valueM =
            calculate model.maxIterations c 0 c
    in
    Maybe.andThen viridisColourMapRefined valueM
        |> Maybe.withDefault (Color.toRgba Color.black)


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
