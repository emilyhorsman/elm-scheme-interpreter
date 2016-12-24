module Main exposing (..)

import Html exposing (Html, textarea, text, div)
import Html.Attributes exposing (value, style)
import Html.Events exposing (onInput)
import Lexer exposing (tokenize)


type alias Model
    = String


model : Model
model =
    "()"


type Msg
    = Change String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change text ->
            text


view : Model -> Html Msg
view model =
    div
        []
        [ textarea
            [ onInput Change
            , value model
            , style
                [ ( "font-family", "monospace" )
                , ( "width", "50%" )
                , ( "height", "40vh" )
                ]
            ]
            []
        , div
            []
            [ model |> tokenize |> toString |> text
            ]
        ]


main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }
