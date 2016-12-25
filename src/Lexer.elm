module Lexer
    exposing
        ( LexerResult(..)
        , tokenize
        , Token(..)
        , Tokens
        )

import Char


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

                ' ' ->
                    state

                otherwise ->
                    if isInitial char then
                        Accumulator tokens (Just [ char ])
                    else
                        Error ("Identifier started with invalid character " ++ String.fromChar char)

        Accumulator tokens (Just buffer) ->
            case ( char, buffer ) of
                ( '(', [ '#' ] ) ->
                    Accumulator (OpenVectorParen :: tokens) Nothing

                ( '(', _ ) ->
                    Error "Opening paren found before identifier completed."

                ( ')', _ ) ->
                    Accumulator (ClosingParen :: getIdentifier buffer :: tokens) Nothing

                ( ' ', _ ) ->
                    Accumulator (getIdentifier buffer :: tokens) Nothing

                otherwise ->
                    Accumulator tokens (Just (char :: buffer))


foldOrAbandon : (a -> b -> b) -> (b -> Bool) -> b -> List a -> b
foldOrAbandon foldFunc abandonPredicate accumulator source =
    case source of
        [] ->
            accumulator

        input :: remaining ->
            let
                result =
                    foldFunc input accumulator
            in
                if abandonPredicate result then
                    result
                else
                    foldOrAbandon foldFunc abandonPredicate result remaining


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
