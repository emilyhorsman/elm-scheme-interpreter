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
            , test "returns identifier with a digit subsequent character" <|
                expect
                    Lexer.tokenize
                    "foo1"
                    (Lexer.LexerSuccess
                        [ Lexer.Identifier "foo1"
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
                    "(foo \t\x0D\n)"
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
            , test "handle strings" <|
                expect
                    Lexer.tokenize
                    "(define foobar \" omg hi!\")"
                    (Lexer.LexerSuccess
                        [ Lexer.OpenParen
                        , Lexer.Identifier "define"
                        , Lexer.Identifier "foobar"
                        , Lexer.Str " omg hi!"
                        , Lexer.ClosingParen
                        ]
                    )
            , test "handle string literals" <|
                expect
                    Lexer.tokenize
                    "(define foobar \"\")"
                    (Lexer.LexerSuccess
                        [ Lexer.OpenParen
                        , Lexer.Identifier "define"
                        , Lexer.Identifier "foobar"
                        , Lexer.Str ""
                        , Lexer.ClosingParen
                        ]
                    )
            , test "handle escaped quote in string" <|
                expect
                    Lexer.tokenize
                    "(define foobar \"\\\"\")"
                    (Lexer.LexerSuccess
                        [ Lexer.OpenParen
                        , Lexer.Identifier "define"
                        , Lexer.Identifier "foobar"
                        , Lexer.Str "\""
                        , Lexer.ClosingParen
                        ]
                    )
            , test "handle escaped backslash in string" <|
                expect
                    Lexer.tokenize
                    """(define foobar "\\\\")"""
                    (Lexer.LexerSuccess
                        [ Lexer.OpenParen
                        , Lexer.Identifier "define"
                        , Lexer.Identifier "foobar"
                        , Lexer.Str "\\"
                        , Lexer.ClosingParen
                        ]
                    )
            , test "error on unfinished token" <|
                isError (Lexer.tokenize "(define \"\"\")")
            , test "handle dotted pair marker" <|
                expect
                    Lexer.tokenize
                    "(a b . c)"
                    (Lexer.LexerSuccess
                        [ Lexer.OpenParen
                        , Lexer.Identifier "a"
                        , Lexer.Identifier "b"
                        , Lexer.DottedPairMarker
                        , Lexer.Identifier "c"
                        , Lexer.ClosingParen
                        ]
                    )
            , test "handle symbol" <|
                expect
                    Lexer.tokenize
                    "'foo"
                    (Lexer.LexerSuccess
                        [ Lexer.Quote
                        , Lexer.Identifier "foo"
                        ]
                    )
            , numbers
            ]
        ]

numbers : Test
numbers =
    describe "numbers"
        [ test "handle base 10 with empty sign" <|
            expect
                Lexer.tokenize
                "42"
                (Lexer.LexerSuccess
                    [ Lexer.Number (Lexer.Exact 42)
                    ]
                )
        , test "handle base 10 with negative sign" <|
            expect
                Lexer.tokenize
                "-42"
                (Lexer.LexerSuccess
                    [ Lexer.Number (Lexer.Exact -42)
                    ]
                )
        , test "handle base 10 with positive sign" <|
            expect
                Lexer.tokenize
                "+42"
                (Lexer.LexerSuccess
                    [ Lexer.Number (Lexer.Exact 42)
                    ]
                )
        , test "handle base 10 with decimal point" <|
            expect
                Lexer.tokenize
                "1.0"
                (Lexer.LexerSuccess
                    [ Lexer.Number (Lexer.Inexact 1.0)
                    ]
                )
        , test "handle base 10 with decimal point as initial" <|
            expect
                Lexer.tokenize
                ".5"
                (Lexer.LexerSuccess
                    [ Lexer.Number (Lexer.Inexact 0.5)
                    ]
                )
        , test "handle base 10 with negative sign and decimal point" <|
            expect
                Lexer.tokenize
                "-.5"
                (Lexer.LexerSuccess
                    [ Lexer.Number (Lexer.Inexact -0.5)
                    ]
                )
        , test "handle + character not part of number" <|
            expect
                Lexer.tokenize
                "(+ a b)"
                (Lexer.LexerSuccess
                    [ Lexer.OpenParen
                    , Lexer.Identifier "+"
                    , Lexer.Identifier "a"
                    , Lexer.Identifier "b"
                    , Lexer.ClosingParen
                    ]
                )
        , test "handle - character not part of number" <|
            expect
                Lexer.tokenize
                "(- a b)"
                (Lexer.LexerSuccess
                    [ Lexer.OpenParen
                    , Lexer.Identifier "-"
                    , Lexer.Identifier "a"
                    , Lexer.Identifier "b"
                    , Lexer.ClosingParen
                    ]
                )
        ]
