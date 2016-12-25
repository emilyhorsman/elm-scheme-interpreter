module TestLexer exposing (all)

import Test exposing (..)
import TestUtil exposing (expect)
import Lexer
import Expect


isError : String -> Lexer.LexerResult -> (() -> Expect.Expectation)
isError message result =
    \() ->
        let
            test =
                case result of
                    Lexer.LexerError _ ->
                        True

                    otherwise ->
                        False
        in
            Expect.true message test


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
            , test "error when opening paren is immediately after identifier" <|
                isError "LexerError expected" (Lexer.tokenize "(foobar(")
            ]
        ]
