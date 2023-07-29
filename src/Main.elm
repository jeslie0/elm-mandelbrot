module Main exposing (..)

import Browser
import Html as H exposing (Attribute, Html)
import Mandelbrot



-- * PORTS

-- * MAIN


main : Program () Mandelbrot.Model Msg
main =
    Browser.sandbox
        { init = Mandelbrot.init 100
        , view = Mandelbrot.view
        , update = \msg model -> model
        }



-- * MODEL

type alias Model =
    {     }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( {      }
    , Cmd.none
    )



-- * UPDATE


type Msg
    = Foo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Foo -> (model, Cmd.none)


-- * SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- * VIEW


viewDocument : Model -> Browser.Document Msg
viewDocument model =
    { title = ""
    , body = [ view model ]
    }


view : Model -> Html Msg
view model =
    H.div [] []
