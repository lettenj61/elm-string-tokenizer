module String.Token exposing (Token(..), Step(..), tokenize)

{-| String tokenization.


# Tokenization

@docs Token, Step, tokenize

-}


{-| -}
type Token a
    = Seq (List a)
    | Elem a


{-| -}
type Step a
    = Append a
    | Capture a
    | Ignore


type alias TokenizeData a =
    { buffer : List (Token a)
    , seq : List a
    }


{-| Tokenize multiline string `str` into some data.

```elm
import String.Token as Token exposing (Step(..))
import Url exposing (Url)

type Parts
  = Plain String
  | Link Url

stepper : String -> Step Parts
stepper str =
  case Url.fromString str of
    Just url ->
      Capture (Link url)

    Nothing ->
      Append (Plain str)

markup : String -> List (Token.Token Parts)
markup str =
  Token.tokenize
    str
    stepper

example : List (Token.Token Parts)
example =
  markup
    """
    Hello, world
    Visit my website https://example.com
    """

-- [ Seq
--   [ Plain "Hello,"
--   , Plain "world"
--   , Plain "Visit"
--   , Plain "my"
--   , Plain "website"
--   ]
-- , Elem (Link "https://example.com")
-- ]
```
-}
tokenize : String -> (String -> Step a) -> List (Token a)
tokenize str stepper =
    let
        result =
            tokenizeHelp
                (toWords str)
                stepper
                { buffer = []
                , seq = []
                }
    in
    List.reverse <|
        case result.seq of
            [] ->
                result.buffer

            x :: xs ->
                mkSequence x xs :: result.buffer


tokenizeHelp : List Word -> (String -> Step a) -> TokenizeData a -> TokenizeData a
tokenizeHelp words stepper state =
    case words of
        [] ->
            state

        next :: more ->
            case next of
                Newline ->
                    tokenizeHelp more stepper state

                Word str ->
                    let
                        newState =
                            case stepper str of
                                Append tok ->
                                    { state
                                        | seq = tok :: state.seq
                                    }

                                Capture tok ->
                                    refresh tok state

                                Ignore ->
                                    state
                    in
                    tokenizeHelp more stepper newState


mkSequence : a -> List a -> Token a
mkSequence tok seq =
    Seq <|
        List.reverse (tok :: seq)


refresh : a -> TokenizeData a -> TokenizeData a
refresh tok state =
    case state.seq of
        [] ->
            { state
                | buffer =
                    Elem tok :: state.buffer
            }

        x :: xs ->
            { state
                | buffer =
                    Elem tok :: mkSequence x xs :: state.buffer
                , seq = []
            }



-- INTERNAL


type Word
    = Word String
    | Newline


toWords : String -> List Word
toWords str =
    List.concatMap
        (\line ->
            List.map Word line ++ [ Newline ]
        )
        (toWordsInternal str)


toWordsInternal : String -> List (List String)
toWordsInternal =
    String.lines >> List.map String.words
