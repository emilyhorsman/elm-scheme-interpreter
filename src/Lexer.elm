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
    | Str String


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
    | InString


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
        isInitial char || List.member char subsequentChars


isWhitespace : Char -> Bool
isWhitespace char =
    char == ' ' || char == '\t' || char == '\n' || char == '\x0D'


checkSpecialChar : List Char -> Maybe Char
checkSpecialChar buffer =
    let
        input =
            -- Buffer comes in as a reversed List Char.
            buffer |> List.reverse |> List.map Char.toLower |> String.fromList
    in
        if input == "newline" then
            Just '\n'
        else if input == "space" then
            Just ' '
        else
            Nothing


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

                '"' ->
                    Accumulator tokens Nothing InString

                otherwise ->
                    if isInitial char then
                        Accumulator tokens (Just [ char ]) Parsing
                    else if isWhitespace char then
                        state
                    else
                        Error ("Identifier started with invalid character `" ++ String.fromChar char ++ "`")

        Accumulator tokens (Just buffer) Parsing ->
            case char of
                '(' ->
                    Error "Opening paren found before identifier completed."

                ')' ->
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

        -- If we are in a #\character it is either a single character or
        -- a special case such as #\newline or #\space.
        Accumulator tokens (Just buffer) InCharacter ->
            if isWhitespace char || char == ')' then
                case checkSpecialChar buffer of
                    Nothing ->
                        Error ("Multi-character character found, `" ++ (buffer |> List.reverse |> String.fromList) ++ "`")

                    Just special ->
                        accumulateTokens char (Accumulator (Character special :: tokens) Nothing Parsing)
            else
                Accumulator tokens (Just (char :: buffer)) InCharacter

        Accumulator tokens buffer InComment ->
            case char of
                '\n' ->
                    Accumulator tokens buffer Parsing

                otherwise ->
                    Accumulator tokens buffer InComment

        Accumulator tokens buffer InString ->
            case ( char, buffer ) of
                ( '"', Just ('\\' :: string) ) ->
                    Accumulator tokens (Just (char :: string)) InString

                ( '"', Just string ) ->
                    Accumulator (Str (string |> List.reverse |> String.fromList) :: tokens) Nothing Parsing

                ( '"', Nothing ) ->
                    Accumulator (Str "" :: tokens) Nothing Parsing

                ( _, Nothing ) ->
                    Accumulator tokens (Just [ char ]) InString

                ( _, Just string ) ->
                    Accumulator tokens (Just (char :: string)) InString


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

            Accumulator tokens Nothing _ ->
                LexerSuccess (List.reverse tokens)

            Accumulator _ (Just buffer) mode ->
                LexerError ("Source not complete, unidentified characters in lexer buffer, `" ++ (buffer |> List.reverse |> String.fromList) ++ "` (" ++ (toString mode) ++ ")")
