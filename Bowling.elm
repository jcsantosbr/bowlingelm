module Bowling exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick, onInput, onCheck)
import Html.Attributes exposing (..)


type alias Pins =
    Int


type alias Roll =
    { rollNumber : Int
    , pins : Pins
    }


type Frame
    = EmptyFrame
    | OngoingFrame Pins
    | NormalFrame Pins Pins
    | Spare Pins Pins Roll
    | Strike Roll
    | OngoingSpare Pins Pins
    | LastFrameSpare Pins Pins Pins
    | OngoingStrikeFistExtra
    | OngoingStrikeSecondExtra Pins
    | LastFrameStrike Pins Pins


type alias ScoredFrame =
    { frame : Frame
    , score : Int
    , sumScore : Int
    }


strikeFrame =
    Strike (Roll 1 10)


spareFrame =
    Spare 4 6 (Roll 2 6)


normalFrame =
    NormalFrame 2 5


ongoingFrame =
    OngoingFrame 7


emptyFrame =
    EmptyFrame


frames =
    [ (ScoredFrame strikeFrame 10 10)
    , (ScoredFrame spareFrame 10 20)
    , (ScoredFrame normalFrame 3 33)
    , (ScoredFrame ongoingFrame 1 34)
    , (ScoredFrame emptyFrame 0 34)
    ]


drawFirstRoll frame =
    text
        (case frame of
            EmptyFrame ->
                ""

            OngoingFrame pins ->
                toString pins

            NormalFrame pins _ ->
                toString pins

            Spare pins _ _ ->
                toString pins

            Strike _ ->
                "10"

            OngoingSpare pins _ ->
                toString pins

            LastFrameSpare pins _ _ ->
                toString pins

            OngoingStrikeFistExtra ->
                "10"

            OngoingStrikeSecondExtra _ ->
                "10"

            LastFrameStrike _ _ ->
                "10"
        )


drawSecondRoll frame =
    text
        (case frame of
            EmptyFrame ->
                ""

            OngoingFrame _ ->
                ""

            NormalFrame _ pins ->
                toString pins

            Spare _ pins _ ->
                toString pins

            Strike _ ->
                "X"

            OngoingSpare _ pins ->
                toString pins

            LastFrameSpare _ pins _ ->
                toString pins

            OngoingStrikeFistExtra ->
                "..."

            OngoingStrikeSecondExtra pins ->
                toString pins

            LastFrameStrike pins _ ->
                toString pins
        )


drawThirdRoll frame =
    text
        (case frame of
            OngoingSpare _ pins ->
                "..."

            LastFrameSpare _ _ pins ->
                toString pins

            OngoingStrikeSecondExtra pins ->
                "..."

            LastFrameStrike _ pins ->
                toString pins

            _ ->
                ""
        )


drawFrame scoredFrame =
    let
        frame =
            scoredFrame.frame

        shouldDisplayThirdRoll =
            (drawThirdRoll scoredFrame.frame) /= text ""

        displayStyleInThirdRoll =
            if shouldDisplayThirdRoll then
                ""
            else
                "None"

        rollClass =
            if shouldDisplayThirdRoll then
                "smallRoll"
            else
                "roll"
    in
        div [ class "frame" ]
            [ div [ class "score-frame" ]
                [ (text (toString scoredFrame.sumScore)) ]
            , div [ class "score-rolls" ]
                [ div [ class rollClass ]
                    [ (drawFirstRoll frame)
                    ]
                , div [ class rollClass ]
                    [ (drawSecondRoll frame)
                    ]
                , div [ (class rollClass), (style [ ( "display", displayStyleInThirdRoll ) ]) ]
                    [ (drawThirdRoll frame)
                    ]
                ]
            ]


drawFrames scoredFrames =
    let
        isNonEmptyFrame scoredFrame =
            case scoredFrame.frame of
                EmptyFrame ->
                    False

                _ ->
                    True

        nonEmptyScoredFrames =
            List.filter isNonEmptyFrame scoredFrames
    in
        List.map drawFrame nonEmptyScoredFrames


drawButton lessPinsThanCurrent numberOfPins =
    let
        isEnabled =
            not (lessPinsThanCurrent numberOfPins)
    in
        button [ (onClick (NewRoll numberOfPins)), (disabled isEnabled) ]
            [ text (toString numberOfPins) ]


drawPanel lessPinsThanCurrent =
    div [ class "panel" ]
        ((button [ onClick NewGame ] [ text "New Game" ])
            :: (List.map
                    (\i -> (drawButton lessPinsThanCurrent i))
                    (List.range 0 10)
               )
        )


isOverMaximumPins currentFrame newPin =
    let
        currentPin =
            case currentFrame of
                OngoingFrame pins ->
                    pins

                _ ->
                    0
    in
        currentPin + newPin <= 10


type alias Model =
    { frames : List ScoredFrame
    , aframes : List Frame
    , rolls : List Roll
    , currentFrame : Frame
    , feedback : String
    }


startGame =
    Model [] [] [] EmptyFrame ""


model =
    startGame


main =
    beginnerProgram { model = model, view = view, update = update }


view model =
    let
        lessPinsThanCurrent =
            isOverMaximumPins model.currentFrame
    in
        div []
            [ div [ class "score" ]
                (drawFrames model.frames)
            , drawPanel lessPinsThanCurrent
            , p [] [ (text model.feedback) ]
            ]


type Msg
    = NewGame
    | NewRoll Int


isFrameComplete frame =
    case frame of
        EmptyFrame ->
            False

        OngoingFrame _ ->
            False

        OngoingSpare _ _ ->
            False

        OngoingStrikeFistExtra ->
            False

        OngoingStrikeSecondExtra _ ->
            False

        _ ->
            True


isGameFinished : List Frame -> Bool
isGameFinished frames =
    let
        completedFrames =
            List.length (List.filter (\f -> isFrameComplete f) frames)
    in
        completedFrames == 10


isLastFrame : List Frame -> Bool
isLastFrame frames =
    let
        completedFrames =
            List.length (List.filter (\f -> isFrameComplete f) frames)
    in
        completedFrames == 9


rollScore : List Roll -> Int -> Int
rollScore rolls number =
    let
        foundRolls =
            List.filter (\r -> r.rollNumber == number) rolls

        foundRoll =
            List.head foundRolls
    in
        case foundRoll of
            Just roll ->
                roll.pins

            Nothing ->
                0


calcFrameScore : Frame -> List Roll -> Int
calcFrameScore frame rolls =
    let
        score =
            case frame of
                Strike roll ->
                    10 + (rollScore rolls (roll.rollNumber + 1)) + (rollScore rolls (roll.rollNumber + 2))

                Spare _ _ roll ->
                    10 + (rollScore rolls (roll.rollNumber + 1))

                NormalFrame f s ->
                    f + s

                OngoingFrame f ->
                    f

                EmptyFrame ->
                    0

                OngoingSpare f s ->
                    f + s

                LastFrameSpare f s t ->
                    f + s + t

                OngoingStrikeFistExtra ->
                    10

                OngoingStrikeSecondExtra s ->
                    10 + s

                LastFrameStrike s t ->
                    10 + s + t
    in
        score


accumScores : Int -> List ( Int, Int ) -> List ( Int, Int )
accumScores score others =
    let
        lastElem =
            List.head others

        lastScore =
            case lastElem of
                Just ( score, elem ) ->
                    score

                Nothing ->
                    0
    in
        ( lastScore + score, score ) :: others


updateScoredFrames : List Frame -> List Roll -> List ScoredFrame
updateScoredFrames frames rolls =
    let
        scores =
            List.map (\f -> calcFrameScore f rolls) frames

        acummulatedScores =
            List.map Tuple.first (List.reverse (List.foldl accumScores [] scores))

        scoredFrames =
            List.map3 ScoredFrame frames scores acummulatedScores
    in
        scoredFrames


updateCurrentFrame : Frame -> Roll -> Bool -> Frame
updateCurrentFrame frame roll lastFrame =
    case ( frame, roll.pins, lastFrame ) of
        ( EmptyFrame, 10, False ) ->
            Strike roll

        ( EmptyFrame, 10, True ) ->
            OngoingStrikeFistExtra

        ( EmptyFrame, f, _ ) ->
            OngoingFrame f

        ( OngoingStrikeFistExtra, pins, _ ) ->
            OngoingStrikeSecondExtra pins

        ( OngoingStrikeSecondExtra f, s, _ ) ->
            LastFrameStrike f s

        ( OngoingSpare f s, t, _ ) ->
            LastFrameSpare f s t

        ( OngoingFrame f, s, True ) ->
            if f + s == 10 then
                OngoingSpare f s
            else
                NormalFrame f s

        ( OngoingFrame f, s, False ) ->
            if f + s == 10 then
                Spare f s roll
            else
                NormalFrame f s

        _ ->
            EmptyFrame


updateFrames : List Frame -> Frame -> ( List Frame, Frame )
updateFrames frames current =
    if isFrameComplete current then
        ( frames ++ [ current ], EmptyFrame )
    else
        ( frames, current )


runNewRoll : Model -> Pins -> Model
runNewRoll model pins =
    if isGameFinished model.aframes then
        model
    else
        let
            newRoll =
                Roll (List.length model.rolls + 1) pins

            rolls =
                newRoll :: model.rolls

            lastFrame =
                isLastFrame model.aframes

            newFrame =
                updateCurrentFrame model.currentFrame newRoll lastFrame

            ( frames, currentFrame ) =
                updateFrames model.aframes newFrame

            scoredFrames =
                updateScoredFrames (frames ++ [ currentFrame ]) rolls

            feedback =
                if isGameFinished frames then
                    "End of game!"
                else
                    ""
        in
            Model scoredFrames frames rolls currentFrame feedback


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewGame ->
            startGame

        NewRoll pins ->
            runNewRoll model pins
