module Example exposing (..)

import Expect exposing (Expectation)
import String.Token as Token exposing (Step(..), Token(..))
import Test exposing (..)


suite : Test
suite =
    todo "Implement our first test. See https://package.elm-lang.org/packages/elm-explorations/test/latest for how to do this!"


type Parts
    = Plain String
    | Link String


fold : List (Token Parts) -> List Parts
fold tokens =
    List.foldl
        (\tok data ->
            case tok of
                Seq s ->
                    let
                        newSeq =
                            List.filterMap
                                (\e ->
                                    case e of
                                        Plain _ ->
                                            Just e

                                        _ ->
                                            Nothing
                                )
                    in
                    data ++ newSeq s

                Elem i ->
                    data ++ [ i ]
        )
        []
        tokens


tokenizer : Test
tokenizer =
    describe "String.Token"
        [ test "tokenize" <|
            \_ ->
                let
                    expected =
                        Debug.todo "x"
                in
                Expect.equal 1 1
        ]


exampleText : String
exampleText =
    String.trim <|
        """
I am example.
This is example too.
You can safely embed URL like https://example.com

https://404.examle.com

Whoa! Suddenly another URL!
        """
