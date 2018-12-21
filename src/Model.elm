module Model exposing (..)

import Maybe exposing (Maybe)
import Random exposing (Seed)
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


type alias Model =
    { difficulty : Difficulty
    , facts : List Datum
    , isHidden : Bool
    , randomSeed : Maybe Seed
    , seed : Seed
    , startTime : Maybe Posix
    , sort : Int
    , started : Bool
    }

type Msg
    = CloseWelcomeScreen
    | FirstRoll Int
    | Roll
    | StartApp Posix
    | Start
    | Reveal
    | ToggleDifficulty

