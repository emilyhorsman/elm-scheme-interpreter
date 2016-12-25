module Lexer
    exposing
        ( LexerResult(..)
        , tokenize
        , Token(..)
        , Tokens
        )

import Char
import Utils exposing (foldOrAbandon)


type Token
    = OpenParen
    | OpenVectorParen
    | ClosingParen
    | Identifier String


type alias Tokens =
    List Token


type LexerResult
    = LexerError String
    | LexerSuccess Tokens


type alias Buffer =
    Maybe (List Char)


type LexerState
    = Accumulator Tokens Buffer
    | Error String


getIdentifier : List Char -> Token
getIdentifier buffer =
    buffer
        |> List.reverse
        |> String.fromList
        |> Identifier


isInitial : Char -> Bool
isInitial char =
    let
        initialChars =
            [ '!'
            , '$'
            , '%'
            , '&'
            , '*'
            , '/'
            , ':'
            , '<'
            , '='
            , '>'
            , '?'
            , '~'
            , '_'
            , '^'
              -- This is a special case for OpenVectorParen
            , '#'
            ]
    in
        (Char.isUpper char
            || Char.isLower char
            || List.member char initialChars
        )


isSubsequent : Char -> Bool
isSubsequent char =
    let
        subsequentChars =
            [ '.'
            , '+'
            , '_'
            ]
    in
        (Char.isUpper char
            || Char.isLower char
            || Char.isDigit char
            || List.member char subsequentChars
        )


isWhitespace : Char -> Bool
isWhitespace char =
    char == ' ' || char == '\t' || char == '\n' || char == '\r'


accumulateTokens : Char -> LexerState -> LexerState
accumulateTokens char state =
    case state of
        Error msg ->
            state

        Accumulator tokens Nothing ->
            case char of
                '(' ->
                    Accumulator (OpenParen :: tokens) Nothing

                ')' ->
                    Accumulator (ClosingParen :: tokens) Nothing

                otherwise ->
                    if isInitial char then
                        Accumulator tokens (Just [ char ])
                    else if isWhitespace char then
                        state
                    else
                        Error ("Identifier started with invalid character `" ++ String.fromChar char ++ "`")

        Accumulator tokens (Just buffer) ->
            case ( char, buffer ) of
                ( '(', [ '#' ] ) ->
                    Accumulator (OpenVectorParen :: tokens) Nothing

                ( '(', _ ) ->
                    Error "Opening paren found before identifier completed."

                ( ')', _ ) ->
                    Accumulator (ClosingParen :: getIdentifier buffer :: tokens) Nothing

                otherwise ->
                    if isSubsequent char then
                        Accumulator tokens (Just (char :: buffer))
                    else if isWhitespace char then
                        Accumulator (getIdentifier buffer :: tokens) Nothing
                    else
                        Error ("Identifier continued with invalid subsequent character `" ++ String.fromChar char ++ "`")


isError : LexerState -> Bool
isError state =
    case state of
        Error _ ->
            True

        otherwise ->
            False


tokenize : String -> LexerResult
tokenize source =
    let
        result =
            source
                |> String.toList
                |> foldOrAbandon accumulateTokens isError (Accumulator [] Nothing)
    in
        case result of
            Error message ->
                LexerError message

            Accumulator tokens _ ->
                LexerSuccess (List.reverse tokens)
