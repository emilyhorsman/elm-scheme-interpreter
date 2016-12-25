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
    | Boolean Bool
    | Character Char


type alias Tokens =
    List Token


type LexerResult
    = LexerError String
    | LexerSuccess Tokens


type alias Buffer =
    Maybe (List Char)


type LexerState
    = Accumulator Tokens Buffer TokenState
    | Error String


type TokenState
    = Parsing
    | InComment
    | OpeningVector
    | InCharacter


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
            , '-'
            ]
    in
        (Char.isUpper char
            || Char.isLower char
            || Char.isDigit char
            || List.member char subsequentChars
        )


isWhitespace : Char -> Bool
isWhitespace char =
    char == ' ' || char == '\t' || char == '\n' || char == '\x0D'


isCaseInsensitiveWord : String -> List Char -> Bool
isCaseInsensitiveWord word buffer =
    (buffer |> List.reverse |> List.map Char.toLower) == String.toList word


accumulateTokens : Char -> LexerState -> LexerState
accumulateTokens char state =
    case state of
        Error msg ->
            state

        Accumulator tokens Nothing Parsing ->
            case char of
                '(' ->
                    Accumulator (OpenParen :: tokens) Nothing Parsing

                ')' ->
                    Accumulator (ClosingParen :: tokens) Nothing Parsing

                ';' ->
                    Accumulator tokens Nothing InComment

                '#' ->
                    Accumulator tokens Nothing OpeningVector

                otherwise ->
                    if isInitial char then
                        Accumulator tokens (Just [ char ]) Parsing
                    else if isWhitespace char then
                        state
                    else
                        Error ("Identifier started with invalid character `" ++ String.fromChar char ++ "`")

        Accumulator tokens (Just buffer) Parsing ->
            case ( char, buffer ) of
                ( '(', _ ) ->
                    Error "Opening paren found before identifier completed."

                ( ')', _ ) ->
                    accumulateTokens ')' (Accumulator (getIdentifier buffer :: tokens) Nothing Parsing)

                otherwise ->
                    if isSubsequent char then
                        Accumulator tokens (Just (char :: buffer)) Parsing
                    else if isWhitespace char then
                        Accumulator (getIdentifier buffer :: tokens) Nothing Parsing
                    else
                        Error ("Identifier continued with invalid subsequent character `" ++ String.fromChar char ++ "`")

        Accumulator tokens buffer OpeningVector ->
            case Char.toLower char of
                '(' ->
                    Accumulator (OpenVectorParen :: tokens) Nothing Parsing

                't' ->
                    Accumulator (Boolean True :: tokens) Nothing Parsing

                'f' ->
                    Accumulator (Boolean False :: tokens) Nothing Parsing

                '\\' ->
                    Accumulator tokens Nothing InCharacter

                otherwise ->
                    Error ("Invalid character followed vector, `" ++ String.fromChar char ++ "`")

        Accumulator tokens Nothing InCharacter ->
            Accumulator tokens (Just [ char ]) InCharacter

        Accumulator tokens (Just (buffer :: [])) InCharacter ->
            if isWhitespace char || char == ')' then
                accumulateTokens char (Accumulator (Character buffer :: tokens) Nothing Parsing)
            else
                Accumulator tokens (Just (char :: buffer :: [])) InCharacter

        Accumulator tokens (Just buffer) InCharacter ->
            if isCaseInsensitiveWord "newline" buffer then
                if isWhitespace char || char == ')' then
                    accumulateTokens char (Accumulator (Character '\n' :: tokens) Nothing Parsing)
                else
                    Error ("Multi-character character found, `" ++ (buffer |> List.reverse |> String.fromList) ++ "`, did you mean `#\\newline`?")
            else if isCaseInsensitiveWord "space" buffer then
                if isWhitespace char || char == ')' then
                    accumulateTokens char (Accumulator (Character ' ' :: tokens) Nothing Parsing)
                else
                    Error ("Multi-character character found, `" ++ (buffer |> List.reverse |> String.fromList) ++ "`, did you mean `#\\space`?")
            else if isWhitespace char || char == ')' then
                Error ("Multi-character character found, `" ++ (buffer |> List.reverse |> String.fromList) ++ "`")
            else
                Accumulator tokens (Just (char :: buffer)) InCharacter

        Accumulator tokens buffer InComment ->
            case char of
                '\n' ->
                    Accumulator tokens buffer Parsing

                otherwise ->
                    Accumulator tokens buffer InComment


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
                |> foldOrAbandon accumulateTokens isError (Accumulator [] Nothing Parsing)
    in
        case result of
            Error message ->
                LexerError message

            Accumulator tokens _ _ ->
                LexerSuccess (List.reverse tokens)
