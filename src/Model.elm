module Model exposing (Category(..), Datum, Difficulty(..), Mode(..), Model, Msg(..), shuffle)

import Maybe exposing (Maybe)
import Random exposing (Generator, Seed, int)
import Time exposing (Posix)


type Difficulty
    = Easy
    | Hard


type Category
    = Building
    | FIFA
    | Hurricane
    | Movie
    | Music
    | NoCategory
    | Olympics
    | Politics
    | Sports
    | Stage
    | Tv


type alias Datum =
    ( Category, Int, String )


type Mode
    = ShowManyQuestions
    | ShowManyAnswers
    | StartGame
    | PlayGame
    | WelcomeScreen


type alias Model =
    { difficulty : Difficulty
    , facts : List Datum
    , isHidden : Bool
    , mode : Mode
    , printSeed : Seed
    , randomSeed : Maybe Seed
    , startTime : Maybe Posix
    , sort : Int
    , started : Bool
    }


type Msg
    = CloseWelcomeScreen
    | FirstRoll Int
    | PrintAnswers
    | PrintQuiz
    | Roll
    | Reveal
    | Start
    | StartApp Posix
    | ToggleDifficulty


{-| stole this from Random.elm, don't know why I couldn't import it
-}
bool : Generator Bool
bool =
    Random.map ((==) 1) (int 0 1)


{-| Randomly order facts
-}
shuffle : Seed -> List a -> List a
shuffle seed list =
    case list of
        [] ->
            []

        x :: xs ->
            let
                ( b, seed1 ) =
                    Random.step bool seed
            in
            if b then
                x :: shuffle seed1 xs

            else
                shuffle seed1 xs ++ [ x ]
