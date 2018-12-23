module View exposing (view)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Model exposing (..)
import String exposing (fromInt)
import Time exposing (posixToMillis)
import Update exposing (getSeed, justOrDefault)


{-| used for a printable version of the quiz
-}
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
                    viewGame model

                ShowManyAnswers ->
                    viewMany model

                ShowManyQuestions ->
                    viewMany model

                StartGame ->
                    viewGame model

                WelcomeScreen ->
                    viewWelcome model
    in
    { title = "What Year Was It?"
    , body = [ b ]
    }


{-| show one item in the quize
-}
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
        [ text (" " ++ event)
        ]
    ]


viewGame : Model -> Html Msg
viewGame model =
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
            , button [ onClick Roll, disabled model.isHidden ] [ text "Next" ]
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


{-| used for a printable version of the quiz
-}
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


viewWelcome : Model -> Html Msg
viewWelcome model =
    div []
        [ p [] [ h1 [] [ text "Welcome!" ] ]
        , button [ onClick CloseWelcomeScreen ] [ text "Ok" ]
        ]
