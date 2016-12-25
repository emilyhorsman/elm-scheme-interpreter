module Utils
    exposing
        ( foldOrAbandon
        )


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
