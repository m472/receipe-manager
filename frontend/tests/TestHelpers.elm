module TestHelpers exposing (..)

import Expect
import Helpers exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "helpers module"
        [ describe "removeElementAt"
            [ test "removes existing element" <|
                \_ ->
                    Expect.equal (removeElementAt 2 [ 1, 3, 12, 2 ]) [ 1, 3, 2 ]
            , test "removes only element" <|
                \_ ->
                    Expect.equal (removeElementAt 0 [ 1 ]) []
            , test "no effect if index is out of range" <|
                \_ ->
                    Expect.equal (removeElementAt 0 []) []
            ]
        , describe "updateElementAt"
            [ test "updates existing element" <|
                \_ ->
                    Expect.equal (updateElementAt 2 (\x -> x + 30) [ 1, 3, 12, 2 ]) [ 1, 3, 42, 2 ]
            , test "does nothing for index out of range" <|
                \_ ->
                    Expect.equal (updateElementAt 123 (\x -> x + 30) [ 1, 3, 12, 2 ]) [ 1, 3, 12, 2 ]
            , test "works on single element" <|
                \_ ->
                    Expect.equal (updateElementAt 0 (\_ -> 4) [ 1 ]) [ 4 ]
            , test "works on empty" <|
                \_ ->
                    Expect.equal (updateElementAt 0 (\_ -> 4) []) []
            ]
        , describe "keep_just"
            [ test "regular mix" <|
                \_ ->
                    Expect.equal
                        (keep_just
                            [ Just 1, Just 3, Nothing, Just 2, Nothing ]
                        )
                        [ 1, 3, 2 ]
            , test "only justs" <|
                \_ ->
                    Expect.equal
                        (keep_just [ Just 1, Just 3, Just 2 ])
                        [ 1, 3, 2 ]
            , test "empty list" <| \_ -> Expect.equal (keep_just []) []
            , test "only nothing" <| \_ -> Expect.equal (keep_just [ Nothing, Nothing ]) []
            ]
        ]
