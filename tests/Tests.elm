module Tests exposing (..)

import Test exposing (..)
import TestLexer


all : Test
all =
    describe "elm-scheme-interpreter"
        [ TestLexer.all
        ]
