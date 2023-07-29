module IncrementalMandelbrot exposing (Model, Msg, init, update, view)

import Mandelbrot
import Task
import Html exposing (Html)
import Process


type alias Model =
    { fractal : Mandelbrot.Model
    , nextRow : Int
    }


init : Int -> ( Model, Cmd Msg )
init size =
    ( { fractal = Mandelbrot.init size
      , nextRow = 0
      }
    , Task.succeed ()
        |> Task.perform
            (always CalculateNextRow)
    )


type Msg
    = CalculateNextRow


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg!" msg of
        CalculateNextRow ->
            if model.nextRow <= model.fractal.width then
                ( { model | fractal = Mandelbrot.computeRow model.nextRow model.fractal, nextRow = model.nextRow + 1 }
                , Process.sleep 0 |> Task.perform (always CalculateNextRow)
                )

            else
                ( model, Cmd.none )


view : Model -> Html msg
view model =
    Mandelbrot.view model.fractal
