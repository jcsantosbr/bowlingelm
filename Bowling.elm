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


type alias ScoredFrame =
    { frame : Frame
    , score : Int
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
    [ (ScoredFrame strikeFrame 10)
    , (ScoredFrame spareFrame 10)
    , (ScoredFrame normalFrame 3)
    , (ScoredFrame ongoingFrame 1)
    , (ScoredFrame emptyFrame 0)
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
        )


drawFrame frame =
    div [ class "frame" ]
        [ div [ class "score-frame" ]
            [ (text (toString frame.score)) ]
        , div [ class "score-rolls" ]
            [ div [ class "roll" ]
                [ (drawFirstRoll frame.frame)
                ]
            , div [ class "roll" ]
                [ (drawSecondRoll frame.frame)
                ]
            ]
        ]


drawFrames frames =
    List.map drawFrame frames


drawPanel =
    div [ class "panel" ]
        ((button [ onClick NewGame ] [ text "New Game" ])
            :: (List.map
                    (\i ->
                        button [ (onClick (NewRoll i)) ]
                            [ text (toString i) ]
                    )
                    (List.range 0 10)
               )
        )


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
    div []
        [ div [ class "score" ]
            (drawFrames (model.frames ++ [ (ScoredFrame model.currentFrame 0) ]))
        , drawPanel
        , p [] [ (text ("Last: " ++ model.feedback)) ]
        ]


type Msg
    = NewGame
    | NewRoll Int


updateCurrentFrame : Frame -> Roll -> Frame
updateCurrentFrame frame roll =
    case ( frame, roll.pins ) of
        ( EmptyFrame, 10 ) ->
            Strike roll

        ( EmptyFrame, f ) ->
            OngoingFrame f

        ( OngoingFrame f, s ) ->
            if f + s == 10 then
                Spare f s roll
            else
                NormalFrame f s

        _ ->
            EmptyFrame


isFrameComplete frame =
    case frame of
        EmptyFrame ->
            False

        OngoingFrame _ ->
            False

        _ ->
            True


updateFrames : List Frame -> Frame -> ( List Frame, Frame )
updateFrames frames current =
    if isFrameComplete current then
        ( frames ++ [ current ], EmptyFrame )
    else
        ( frames, current )


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


calcFrameScore : Frame -> List Roll -> ScoredFrame
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
    in
        ScoredFrame frame score


updateScoredFrames : List Frame -> List Roll -> List ScoredFrame
updateScoredFrames frames rolls =
    List.map (\f -> calcFrameScore f rolls) frames


runNewRoll : Model -> Pins -> Model
runNewRoll model pins =
    let
        newRoll =
            Roll (List.length model.rolls + 1) pins

        rolls =
            newRoll :: model.rolls

        newFrame =
            updateCurrentFrame model.currentFrame newRoll

        ( frames, currentFrame ) =
            updateFrames model.aframes newFrame

        scoredFrames =
            updateScoredFrames frames rolls

        feedback =
            toString (frames ++ [ currentFrame ])
    in
        Model scoredFrames frames rolls currentFrame feedback


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewGame ->
            startGame

        NewRoll pins ->
            runNewRoll model pins
