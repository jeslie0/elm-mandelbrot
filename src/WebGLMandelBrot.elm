module WebGLMandelBrot exposing (main)

import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onResize)
import Html as H exposing (Html)
import Html.Attributes as HA
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector4 exposing (Vec4, vec4)
import String exposing (fromInt)
import Task
import WebGL exposing (Mesh, Shader)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- * MODEL


type alias Model =
    { screenWidth : Float
    , screenHeight : Float
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { screenWidth = 400
      , screenHeight = 400
      }
    , Task.attempt
        (\result ->
            case result of
                Ok val ->
                    GetScreenDimensions
                        { width = val.viewport.width
                        , height = val.viewport.height
                        }

                Err _ ->
                    GetScreenDimensions { width = 400, height = 400 }
        )
        getViewport
    )



-- * UPDATE


type Msg
    = GetScreenDimensions { width : Float, height : Float }


subscriptions : Model -> Sub Msg
subscriptions model =
    onResize (\w h -> GetScreenDimensions { width = toFloat w, height = toFloat h })


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetScreenDimensions { width, height } ->
            ( { model | screenWidth = width, screenHeight = height }, Cmd.none )



-- * VIEW


view : Model -> Html Msg
view model =
    WebGL.toHtml
        [ HA.width <| round model.screenWidth
        , HA.height <| round model.screenHeight
        , HA.style "overflow" "disabled"
        ]
        [ WebGL.entity
            vertexShader
            fragmentShader
            mesh
            { screenWidth = model.screenWidth
            , screenHeight = model.screenHeight
            , xOffset = 0
            , yOffset = 0
            , zoom = 1.1
            }
        ]


type alias Vertex =
    { position : Vec2 }


mesh : Mesh Vertex
mesh =
    -- These determine the pixel position
    WebGL.triangles
        [ ( Vertex (vec2 -1 -1)
          , Vertex (vec2 1 -1)
          , Vertex (vec2 -1 1)
          )
        , ( Vertex (vec2 -1 1)
          , Vertex (vec2 1 -1)
          , Vertex (vec2 1 1)
          )
        ]


type alias Uniform =
    { screenWidth : Float
    , screenHeight : Float
    , xOffset : Float
    , yOffset : Float
    , zoom : Float
    }


type alias Varying =
    { vpos : Vec2
    }


vertexShader : Shader Vertex Uniform Varying
vertexShader =
    [glsl|

         precision highp float;
         attribute vec2 position;
         uniform float screenWidth;
         uniform float screenHeight;
         uniform float zoom;
         varying vec2 vpos;

         void main()
         {
             float aspect_ratio = screenWidth / screenHeight;
             gl_Position = vec4(position.x, position.y, 0.0, 1);
             vpos = vec2((position.x - 0.1) * 1.5 / zoom * aspect_ratio, position.y * 1.5 / zoom);
         }

|]


fragmentShader : Shader {} Uniform Varying
fragmentShader =
    [glsl|

         precision highp float;
         varying vec2 vpos;

precision mediump float;

vec2 u_zoomCenter = vec2(0.0, 0.0);

float u_zoomSize = 5.0;

const int u_maxIterations = 40;

vec2 mandelbrot_function(vec2 z, vec2 c) {
    vec2 new_z;
    new_z.x = z.x * z.x - z.y * z.y + c.x;
    new_z.y = 2.0 * z.x * z.y + c.y;
    return new_z;
}

vec3 TurboColormap(in float x) {
  const vec4 kRedVec4 = vec4(0.13572138, 4.61539260, -42.66032258, 132.13108234);
  const vec4 kGreenVec4 = vec4(0.09140261, 2.19418839, 4.84296658, -14.18503333);
  const vec4 kBlueVec4 = vec4(0.10667330, 12.64194608, -60.58204836, 110.36276771);
  const vec2 kRedVec2 = vec2(-152.94239396, 59.28637943);
  const vec2 kGreenVec2 = vec2(4.27729857, 2.82956604);
  const vec2 kBlueVec2 = vec2(-89.90310912, 27.34824973);

  x = clamp(x, 0.0, 1.0);
  vec4 v4 = vec4( 1.0, x, x * x, x * x * x);
  vec2 v2 = v4.zw * v4.z;
  return vec3(
    dot(v4, kRedVec4)   + dot(v2, kRedVec2),
    dot(v4, kGreenVec4) + dot(v2, kGreenVec2),
    dot(v4, kBlueVec4)  + dot(v2, kBlueVec2)
  );
}

float better_iteration(int n, vec2 z)
{
    return float(n) + 1.0 - log(log(length(z))) / (log(2.0));
}


void main()
{

    vec2 c = vpos;


    vec2 z = vec2(0.0);
    bool escaped = false;
    int iterations = 0;
    for (int i = 0; i < u_maxIterations; i++) {
        iterations = i;
        if (i > u_maxIterations) {
            break;
        }

        z = mandelbrot_function(z, c);
        if (length(z) > 2.0) {
            escaped = true;
            break;
        }
    }

    gl_FragColor = vec4(TurboColormap(better_iteration(iterations, z) / float(u_maxIterations)), 1.0);
}

    |]



-- precision mediump float;
-- vec2 u_zoomCenter = vec2(0.0, 0.0);
-- float u_zoomSize = 5.0;
-- varying vec2 v_pos;
-- const int u_maxIterations = 100;
-- vec2 f(vec2 z, vec2 c) {
--     vec2 new_z;
--     new_z.x = z.x * z.x - z.y * z.y + c.x;
--     new_z.y = 2.0 * z.x * z.y + c.y;
--     return new_z;
-- }
-- vec2 uv = v_pos.xy / iResolution.xy;
-- vec2 c = u_zoomCenter + (uv * 4.0 - vec2(2.0)) * (u_zoomSize / 4.0);
-- vec2 z = vec2(0.0);
-- bool escaped = false;
-- for (int i = 0; i < u_maxIterations; i++) {
--     if (i > u_maxIterations) {
--         break;
--     }
--     z = f(z, c);
--     if (length(z) > 2.0) {
--         escaped = true;
--         break;
--     }
-- }
-- gl_FragColor = escaped ? vec4(1.0) : vec4(vec3(0.0), 1.0);
