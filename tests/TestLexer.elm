module TestLexer exposing (all)

import Test exposing (..)
import TestUtil exposing (expect)
import Lexer


all : Test
all =
    describe "lexer"
        [ describe "tokenize"
            [ test "returns blank token list" <|
                expect
                    Lexer.tokenize
                    ""
                    (Lexer.LexerSuccess [])
            , test "returns identifier" <|
                expect
                    Lexer.tokenize
                    "(foobar)"
                    (Lexer.LexerSuccess
                        [ Lexer.OpenParen
                        , Lexer.Identifier "foobar"
                        , Lexer.ClosingParen
                        ]
                    )
            ]
        ]
