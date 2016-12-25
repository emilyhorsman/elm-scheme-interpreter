module TestLexer exposing (all)

import Test exposing (..)
import TestUtil exposing (expect)
import Lexer
import Expect


isError : Lexer.LexerResult -> (() -> Expect.Expectation)
isError result =
    \() ->
        let
            test =
                case result of
                    Lexer.LexerError _ ->
                        True

                    otherwise ->
                        False
        in
            Expect.true ("LexerError expected, got " ++ (toString result)) test


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
                isError (Lexer.tokenize "(foobar(")
            , test "returns open vector paren" <|
                expect
                    Lexer.tokenize
                    "#()"
                    (Lexer.LexerSuccess
                        [ Lexer.OpenVectorParen
                        , Lexer.ClosingParen
                        ]
                    )
            , test "error when identifier starts with invalid character" <|
                isError (Lexer.tokenize "({foo)")
            ]
        ]
