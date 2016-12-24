module Main exposing (..)

import Html exposing (text)
import Lexer exposing (tokenize)


source =
    "(!input+ ())"


main =
    source
        |> tokenize
        |> toString
        |> text
