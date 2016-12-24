module Lexer
    exposing
        ( LexerResult(..)
        , tokenize
        , Token(..)
        , Tokens
        )


type Token
    = OpenParen
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
                    Accumulator tokens (Just [ char ])

        Accumulator tokens (Just buffer) ->
            case char of
                '(' ->
                    Error "Opening paren before identifier completed."

                ')' ->
                    Accumulator (getIdentifier buffer :: tokens) Nothing

                ' ' ->
                    Accumulator (getIdentifier buffer :: tokens) Nothing

                otherwise ->
                    Accumulator tokens (Just (char :: buffer))


foldOrAbandon : (Char -> LexerState -> LexerState) -> (LexerState -> Bool) -> LexerState -> List Char -> LexerState
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
                else if List.isEmpty remaining then
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
