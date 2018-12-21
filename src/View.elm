module View exposing (..)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Model exposing (..)
import Random exposing (Generator, Seed, int, map, maxInt, minInt, step)
import String exposing (fromInt)
import Time exposing (posixToMillis)
import Update exposing (justOrDefault)

{- stole this from Random.elm, don't know why I couldn't import it -}
bool : Generator Bool
bool =
  Random.map ((==) 1) (int 0 1)

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

view : Model -> Document Msg
view model =
    let 
        b =  case model.startTime of
            Nothing ->
                viewWelcome model

            Just t ->
                viewStuff model
    in 
    { title = "Elm App Template"
    , body =  [b]
    }

viewWelcome : Model -> Html Msg
viewWelcome model =
    div []
        [ p [] [ h1 [] [ text "Welcome!" ] ]
        , button [ onClick CloseWelcomeScreen ] [ text "Ok" ]
        ]


viewStuff : Model -> Html Msg
viewStuff model =
    let
        ( c, matchYear, e ) =
            justOrDefault (List.head model.facts) ( NoCategory, 0, "This never should happen!" )
    in
        div
                [ style "position" "absolute"
                , style "font-size" "100"
                , style "top" "50px"
                , style "left" "100px"
                ]
            (if model.started then
                [ h1 [ style "font-size" "300%" ] [ text "Which two events happened in the same year?" ]
                , button [ onClick Roll, disabled (model.isHidden) ] [ text "Roll" ]
                , button [ onClick Reveal, disabled (not model.isHidden) ] [ text "Show Years" ]
                , button [ onClick ToggleDifficulty ]
                    [ text
                        (if model.difficulty == Easy then
                            "Easy"
                         else
                            "Hard"
                        )
                    ]
                ]
                    ++ List.concatMap
                        (viewDatum matchYear model.isHidden)
                        (shuffle model.seed model.facts)
             else
                [ button [ onClick Start ] [ text "Start" ]
                ]
            )


viewDatum : Int -> Bool -> Datum -> List (Html Msg)
viewDatum matchYear isHidden ( category, year, event ) =
    let
        color =
            if matchYear == year && not isHidden then
                [ style "background-color" "Bisque" ]
            else
                []
    in
        [ p [] []
        , h2 []
            [ text
                (if isHidden then
                    "----"
                 else
                    fromInt (year)
                )
            ]
        , h1 color
            [ text (" " ++ Debug.log "event" event)
            ]
        ]
