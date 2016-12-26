module Lexer
    exposing
        ( Exactness(..)
        , LexerResult(..)
        , tokenize
        , Token(..)
        , Tokens
        )

import Char
import Utils exposing (foldOrAbandon)
import Debug exposing (log)


type Exactness
    = Exact Int
    | Inexact Float


type Token
    = OpenParen
    | OpenVectorParen
    | ClosingParen
    | Identifier String
    | Boolean Bool
    | Character Char
    | Str String
    | DottedPairMarker
    | Quote
    | Number Exactness


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
    | InStringEscape
    | InExactNumber
    | InInexactNumber


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


isNumberInitial : Char -> Bool
isNumberInitial char =
    Char.isDigit char || char == '+' || char == '-'


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


{-| String.to{Int,Float} does not accept a String prefixed with
    a `+` character. This might be better done by keeping some more
    state in the lexer but this is easy for now.
-}
stripPlusSign : List Char -> List Char
stripPlusSign chars =
    case chars of
        '+' :: remaining ->
            remaining

        otherwise ->
            otherwise


stateFromFloatBuffer : List Char -> Tokens -> LexerState
stateFromFloatBuffer buffer tokens =
    let
        result =
            buffer
                |> List.reverse
                |> stripPlusSign
                |> String.fromList
                |> String.toFloat
    in
        case result of
            Ok num ->
                Accumulator (Number (Inexact num) :: tokens) Nothing Parsing

            Err msg ->
                Error ("Invalid inexact number, " ++ msg)


stateFromIntBuffer : List Char -> Tokens -> LexerState
stateFromIntBuffer buffer tokens =
    let
        result =
            buffer
                |> List.reverse
                |> stripPlusSign
                |> String.fromList
                |> String.toInt
    in
        case result of
            Ok num ->
                Accumulator (Number (Exact num) :: tokens) Nothing Parsing

            Err msg ->
                Error ("Invalid exact number, " ++ msg)


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

                -- InInexactNumber is our best guess, it could be a
                -- DottedPairMarker if its the sole character.
                '.' ->
                    Accumulator tokens (Just [ '.' ]) InInexactNumber

                '\'' ->
                    Accumulator (Quote :: tokens) Nothing Parsing

                otherwise ->
                    if isInitial char then
                        Accumulator tokens (Just [ char ]) Parsing
                    else if isWhitespace char then
                        state
                    else if isNumberInitial char then
                        Accumulator tokens (Just [ char ]) InExactNumber
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
                ( '\\', _ ) ->
                    Accumulator tokens buffer InStringEscape

                ( '"', Just string ) ->
                    Accumulator (Str (string |> List.reverse |> String.fromList) :: tokens) Nothing Parsing

                ( '"', Nothing ) ->
                    Accumulator (Str "" :: tokens) Nothing Parsing

                ( _, Nothing ) ->
                    Accumulator tokens (Just [ char ]) InString

                ( _, Just string ) ->
                    Accumulator tokens (Just (char :: string)) InString

        Accumulator tokens buffer InStringEscape ->
            case buffer of
                Nothing ->
                    Accumulator tokens (Just [ char ]) InString

                Just string ->
                    Accumulator tokens (Just (char :: string)) InString

        Accumulator tokens (Just buffer) InExactNumber ->
            case char of
                '.' ->
                    Accumulator tokens (Just (char :: buffer)) InInexactNumber

                otherwise ->
                    if isWhitespace char || char == ')' then
                        stateFromIntBuffer buffer tokens
                    else
                        Accumulator tokens (Just (char :: buffer)) InExactNumber

        Accumulator tokens (Just buffer) InInexactNumber ->
            if isWhitespace char || char == ')' then
                -- jk lol this is a dotted pair marker, not an inexact
                -- number.
                if buffer == [ '.' ] then
                    Accumulator (DottedPairMarker :: tokens) Nothing Parsing
                else
                    stateFromFloatBuffer buffer tokens
            else
                Accumulator tokens (Just (char :: buffer)) InInexactNumber

        Accumulator tokens Nothing state ->
            Error ("Unexpected state reached with blank buffer, " ++ (toString state))


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
                |> accumulateTokens '\n'
    in
        case result of
            Error message ->
                LexerError message

            Accumulator tokens Nothing _ ->
                LexerSuccess (List.reverse tokens)

            Accumulator _ (Just buffer) mode ->
                LexerError ("Source not complete, unidentified characters in lexer buffer, `" ++ (buffer |> List.reverse |> String.fromList) ++ "` (" ++ (toString mode) ++ ")")
