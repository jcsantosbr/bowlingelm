module Bowling exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick, onInput, onCheck)
import Html.Attributes exposing (..)


type alias Pins =
    Int


type Roll
    = EmptyRoll
    | Skipped
    | Rolled Pins


type Frame
    = EmptyFrame
    | OngoingFrame Pins
    | NormalFrame Pins Pins
    | Spare Pins Pins
    | Strike


type alias ScoredFrame =
    { frame : Frame
    , score : Int
    }


strikeFrame =
    Strike


spareFrame =
    Spare 4 6


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

            Spare pins _ ->
                toString pins

            Strike ->
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

            Spare _ pins ->
                toString pins

            Strike ->
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
                        button [ (onClick (Roll i)) ]
                            [ text (toString i) ]
                    )
                    (List.range 0 10)
               )
        )


type alias Model =
    { frames : List ScoredFrame
    , plays : List Pins
    , currentFrame : Frame
    , feedback : String
    }


startGame =
    Model [] [] ""


model =
    startGame


main =
    beginnerProgram { model = model, view = view, update = update }


view model =
    div []
        [ div [ class "score" ]
            (drawFrames model.frames)
        , drawPanel
        , p [] [ (text ("Last: " ++ model.feedback)) ]
        ]


type Msg
    = NewGame
    | Roll Int


playsToFramedRolls pins =
    let
        playsToFramedRollsInt current pins =
            case pins of
                10 :: tail ->
                    playsToFramedRollsInt (( Rolled 10, Skipped ) :: current) tail

                a :: b :: tail ->
                    playsToFramedRollsInt (( Rolled a, Rolled b ) :: current) tail

                a :: tail ->
                    playsToFramedRollsInt (( Rolled a, EmptyRoll ) :: current) tail

                _ ->
                    current
    in
        List.reverse (playsToFramedRollsInt [] pins)



-- [1] -> [(1, _)]
-- [1,2] -> [(1,2)]
-- [1,2,3] -> [(1,2),(3,_)]
-- [10,2,3] -> [(10,_),(2,3)]
-- [0, 10,2,3] -> [(0,10),(2,3)]


runNewRoll : Model -> Pins -> Model
runNewRoll model pins =
    let
        plays =
            model.plays ++ [ pins ]

        feedback =
            (toString (playsToFramedRolls plays))

        frames =
            playsToScoredFrames plays
    in
        Model frames plays feedback


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewGame ->
            startGame

        Roll pins ->
            runNewRoll model pins
