module Main exposing (..)

import Html exposing (Html, textarea, text, div, ol, li)
import Html.Attributes exposing (value, style)
import Html.Events exposing (onInput)
import Lexer exposing (tokenize, LexerResult(..), Token)


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
        , renderTokens model
        ]


renderToken : Token -> Html a
renderToken token =
    li [] [ token |> toString |> text ]

renderTokens : Model -> Html a
renderTokens model =
    let
        tokens =
            tokenize model
    in
        case tokens of
            LexerError message ->
                text message

            LexerSuccess tokens ->
                ol [] (List.map renderToken tokens)


main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }
