module View exposing (view, viewDatum, viewStuff, viewWelcome)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Model exposing (..)
import Random exposing (Generator, Seed, int, map, maxInt, minInt, step)
import String exposing (fromInt)
import Time exposing (posixToMillis)
import Update exposing (getSeed, justOrDefault)


printDatum : Int -> Bool -> Datum -> List (Html Msg)
printDatum matchYear isHidden ( category, year, event ) =
    [ p []
        [ text
            (if isHidden then
                "----"

             else
                fromInt year
            )
        , text (" " ++ event)
        ]
    ]


view : Model -> Document Msg
view model =
    let
        b =
            case model.mode of
                PlayGame ->
                    viewStuff model

                ShowManyAnswers ->
                    viewMany model

                ShowManyQuestions ->
                    viewMany model

                StartGame ->
                    viewStuff model

                WelcomeScreen ->
                    viewWelcome model
    in
    { title = "Elm App Template"
    , body = [ b ]
    }


viewWelcome : Model -> Html Msg
viewWelcome model =
    div []
        [ p [] [ h1 [] [ text "Welcome!" ] ]
        , button [ onClick CloseWelcomeScreen ] [ text "Ok" ]
        ]


viewMany : Model -> Html Msg
viewMany model =
    let
        ( c, matchYear, e ) =
            justOrDefault (List.head model.facts) ( NoCategory, 0, "This never should happen!" )
    in
    div
        [ style "font-size" "10"
        , style "top" "50px"
        , style "left" "20px"
        ]
        ([ button [ onClick PrintQuiz ] [ text "More" ]
         , button [ onClick PrintAnswers ] [ text "Show Years" ]
         , p [] [ text "Which two events happened in the same year?" ]
         ]
            ++ List.concatMap
                (printDatum matchYear model.isHidden)
                model.facts
        )


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
            , button [ onClick Roll, disabled model.isHidden ] [ text "Roll" ]
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
                    (shuffle (getSeed model) model.facts)

         else
            [ button [ onClick Start ] [ text "Start" ]
            , button [ onClick PrintQuiz ] [ text "PrintQuiz" ]
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
                fromInt year
            )
        ]
    , h1 color
        [ text (" " ++ Debug.log "event" event)
        ]
    ]
