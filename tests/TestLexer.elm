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
            , test "error when identifier has an invalid subsequent character" <|
                isError (Lexer.tokenize "(!})")
            , test "subsequent characters include initial characters" <|
                expect
                    Lexer.tokenize
                    "(foo-bar?)"
                    (Lexer.LexerSuccess
                        [ Lexer.OpenParen
                        , Lexer.Identifier "foo-bar?"
                        , Lexer.ClosingParen
                        ]
                    )
            , test "handle all whitespace" <|
                expect
                    Lexer.tokenize
                    "(foo \t\r\n)"
                    (Lexer.LexerSuccess
                        [ Lexer.OpenParen
                        , Lexer.Identifier "foo"
                        , Lexer.ClosingParen
                        ]
                    )
            , test "handle comments" <|
                expect
                    Lexer.tokenize
                    "; foobar\n()"
                    (Lexer.LexerSuccess
                        [ Lexer.OpenParen
                        , Lexer.ClosingParen
                        ]
                    )
            , test "handle boolean true datum" <|
                expect
                    Lexer.tokenize
                    "(define is-allowed #t)"
                    (Lexer.LexerSuccess
                        [ Lexer.OpenParen
                        , Lexer.Identifier "define"
                        , Lexer.Identifier "is-allowed"
                        , Lexer.Boolean True
                        , Lexer.ClosingParen
                        ]
                    )
            , test "handle boolean false datum" <|
                expect
                    Lexer.tokenize
                    "(define is-allowed #F)"
                    (Lexer.LexerSuccess
                        [ Lexer.OpenParen
                        , Lexer.Identifier "define"
                        , Lexer.Identifier "is-allowed"
                        , Lexer.Boolean False
                        , Lexer.ClosingParen
                        ]
                    )
            , test "handle escaped character" <|
                expect
                    Lexer.tokenize
                    "(define foobar #\\ƛ)"
                    (Lexer.LexerSuccess
                        [ Lexer.OpenParen
                        , Lexer.Identifier "define"
                        , Lexer.Identifier "foobar"
                        , Lexer.Character 'ƛ'
                        , Lexer.ClosingParen
                        ]
                    )
            , test "handle newline character" <|
                expect
                    Lexer.tokenize
                    "(define foobar #\\newline)"
                    (Lexer.LexerSuccess
                        [ Lexer.OpenParen
                        , Lexer.Identifier "define"
                        , Lexer.Identifier "foobar"
                        , Lexer.Character '\n'
                        , Lexer.ClosingParen
                        ]
                    )
            , test "handle space character" <|
                expect
                    Lexer.tokenize
                    "(define foobar #\\space)"
                    (Lexer.LexerSuccess
                        [ Lexer.OpenParen
                        , Lexer.Identifier "define"
                        , Lexer.Identifier "foobar"
                        , Lexer.Character ' '
                        , Lexer.ClosingParen
                        ]
                    )
            , test "error on multi-line vector character" <|
                isError (Lexer.tokenize "(define foobar #\\newlinee)")
            ]
        ]
