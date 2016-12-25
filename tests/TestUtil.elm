module TestUtil exposing (expect)

import Expect


expect : (a -> b) -> a -> b -> (() -> Expect.Expectation)
expect func input expected =
    \() -> Expect.equal expected (func input)

