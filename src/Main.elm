module Main exposing (..)

import Browser
import Html as H exposing (Attribute, Html)
import Html.Lazy as Lazy
import Mandelbrot
import IncrementalMandelbrot
import ViewHtml
import ViewCanvas



-- * PORTS

-- * MAIN


main : Program () IncrementalMandelbrot.Model IncrementalMandelbrot.Msg
main =
    Browser.document
        { init = \_ -> IncrementalMandelbrot.init 200
        , view = \model -> {title = "", body = [Lazy.lazy ViewHtml.view model.fractal]}
        , update = IncrementalMandelbrot.update
        , subscriptions = always Sub.none
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
