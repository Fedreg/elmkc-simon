port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import String exposing (..)
import List.Extra exposing (getAt)
import Time exposing (..)
import Update.Extra.Infix exposing ((:>))
import Random
import Task exposing (succeed)
import Process exposing (sleep)


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



--MODEL


type alias Model =
    { noteOptions : List String
    , noteOptions2 : List String
    , computerNotes : List Int
    , playerNotes : List Int
    , computerNoteIndex : Int
    , playerNoteIndex : Int
    , bpm : Int
    , score : Int
    , gameOn : Bool
    , playersTurn : Bool
    }


type alias Note =
    { hz : Float
    , duration : Float
    , octave : Int
    }


type alias PlayBundle =
    { note : Note
    , tempo : Float
    }


model =
    { noteOptions = [ "", "cq4", "d#q4", "fq4", "gq4", "a#q4", "cq5" ]
    , noteOptions2 = [ "", "fq3", "gq3", "aq3", "cq3", "cq4", "eq4" ]
    , computerNotes = []
    , playerNotes = []
    , computerNoteIndex = 0
    , playerNoteIndex = 0
    , bpm = 80
    , score = 0
    , gameOn = False
    , playersTurn = False
    }


init =
    ( model, Cmd.none )



--UPDATE


port send : PlayBundle -> Cmd msg


port blink : Int -> Cmd msg


type Msg
    = StartGame
    | GenerateRandomNote
    | AddNoteToComputerNotes Int
    | AcceptNotesFromPlayer Int
    | CompareResults
    | SendNotes
    | LightUpDiv Int


update msg model =
    case msg of
        StartGame ->
            ( { model
                | computerNotes = []
                , playerNotes = []
                , computerNoteIndex = 0
                , playerNoteIndex = 0
                , score = 0
                , bpm = 160
                , gameOn = True
              }
            , Cmd.none
            )
                :> update GenerateRandomNote

        GenerateRandomNote ->
            ( model, Random.generate AddNoteToComputerNotes (Random.int 1 6) )

        AddNoteToComputerNotes index ->
            let
                noteList =
                    [ index ]
            in
                ( { model | computerNotes = model.computerNotes ++ noteList, computerNoteIndex = 0, playerNotes = [] }, Cmd.none )
                    --:> update (LightUpDiv index)
                    :>
                        update SendNotes

        LightUpDiv id ->
            ( model, blink id )

        AcceptNotesFromPlayer id ->
            let
                noteList =
                    [ id ]
            in
                ( { model | playerNotes = model.playerNotes ++ noteList, playersTurn = True }, Cmd.none )
                    :> update (LightUpDiv id)
                    :> update SendNotes
                    :> update CompareResults

        CompareResults ->
            if model.playerNotes == model.computerNotes then
                ( { model | bpm = model.bpm + 10, score = model.score + 1 }, delayCmd )
            else if getAt (model.playerNoteIndex - 1) model.playerNotes /= getAt (model.playerNoteIndex - 1)  model.computerNotes then
                ( { model | gameOn = False }, Cmd.none )
            else
                ( model, Cmd.none )

        --:> update StartGame
        SendNotes ->
            if model.playersTurn == False then
                let
                    position =
                        getAt model.computerNoteIndex model.computerNotes

                    note =
                        getAt (Maybe.withDefault 0 position) model.noteOptions

                    note2 =
                        getAt (Maybe.withDefault 0 position) model.noteOptions2
                in
                    ( { model | computerNoteIndex = model.computerNoteIndex + 1, playersTurn = True, playerNoteIndex = 0 }
                    , Cmd.batch
                        [ send (PlayBundle (noteSorter <| Maybe.withDefault "cq4" note) (tempo model.bpm))
                        , send (PlayBundle (noteSorter <| Maybe.withDefault "cq4" note2) (tempo model.bpm))
                        ]
                    )
                        :> update (LightUpDiv (Maybe.withDefault 1 position))
            else
                let
                    position =
                        getAt model.playerNoteIndex model.playerNotes

                    note =
                        getAt (Maybe.withDefault 0 position) model.noteOptions

                    note2 =
                        getAt (Maybe.withDefault 0 position) model.noteOptions2
                in
                    ( { model | playerNoteIndex = model.playerNoteIndex + 1, playersTurn = False }
                    , Cmd.batch
                        [ send (PlayBundle (noteSorter <| Maybe.withDefault "cq4" note) (tempo model.bpm))
                        , send (PlayBundle (noteSorter <| Maybe.withDefault "cq4" note2) (tempo model.bpm))
                        ]
                    )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.gameOn == True && model.computerNoteIndex < List.length model.computerNotes then
        let
            speed =
                tempo model.bpm
        in
            Time.every (speed * second) (always SendNotes)
    else
        Sub.none


noteSorter : String -> Note
noteSorter string =
    let
        _ =
            Debug.log "Note_" string
    in
        case (String.length string) of
            3 ->
                Note (frequencies (slice 0 1 string)) (sustain (slice 1 2 string)) (octave (Result.withDefault 0 (toInt (slice 2 3 string))))

            4 ->
                Note (frequencies (slice 0 2 string)) (sustain (slice 2 3 string)) (octave (Result.withDefault 0 (toInt (slice 3 4 string))))

            _ ->
                Note 0.0 0.0 0


sustain : String -> Float
sustain duration =
    case duration of
        "w" ->
            4.0

        "h" ->
            2.0

        "q" ->
            1.0

        "e" ->
            0.5

        "s" ->
            0.25

        _ ->
            0.0


octave : Int -> Int
octave num =
    case num of
        1 ->
            1

        _ ->
            2 ^ (num - 1)


frequencies : String -> Float
frequencies note =
    case note of
        "c" ->
            130.81

        "c#" ->
            139.0

        "d" ->
            146.83

        "d#" ->
            156.0

        "e" ->
            164.81

        "f" ->
            174.61

        "f#" ->
            185.0

        "g" ->
            196.0

        "g#" ->
            208.0

        "a" ->
            220.0

        "a#" ->
            233.0

        "b" ->
            246.94

        "r" ->
            0.0

        _ ->
            0.0



--Formula for determining frequencies in hz
--110 * (1.059463..)^n


tempo : Int -> Float
tempo bpm =
    (Basics.toFloat 60 / Basics.toFloat bpm) * 0.5


delayCmd =
    Task.perform identity
        (Process.sleep (1 * Time.second)
            |> Task.andThen (\() -> Task.succeed GenerateRandomNote)
        )



-- VIEW


view : Model -> Html Msg
view model =
    div [ style [ ( "textAlign", "center" ), ( "color", "#555" ) ] ]
        [ h1 [ style [ ( "textDecoration", "underline" ), ( "margin", "150px auto 50px" ) ] ] [ text "Elm Simon" ]
        , button [ onClick StartGame, myStyles ] [ text "Start Game" ]
        , div [ style [ ( "display", "flex" ) ] ]
            [ colorNoteDiv 1 "#E8F1F2"
            , colorNoteDiv 2 "#8338EC"
            , colorNoteDiv 3 "#3A86FF"
            , colorNoteDiv 4 "#FF006E"
            , colorNoteDiv 5 "#FFBE0B"
            , colorNoteDiv 6 "#FB5607"
            ]
        , score model
        ]


colorNoteDiv : Int -> String -> Html Msg
colorNoteDiv index color =
    div
        [ id (Basics.toString index)
        , style [ ( "width", "30%" ), ( "height", "50px" ), ( "border", "3px solid " ++ color ), ( "borderRadius", "7px" ), ( "margin", "25px" ) ]
        , onClick (AcceptNotesFromPlayer index)
        ]
        []


score model =
    if
        model.gameOn== True        
    then
        h2 [ style [ ( "margin", "0 auto" ) ] ] [ text ("Score: " ++ Basics.toString model.score) ]
    else
        h2 [ style [ ( "margin", "0 auto" ), ( "color", "#E8F1F2" ) ] ] [ text ("Game Over! Score: " ++ Basics.toString model.score) ]


myStyles : Attribute msg
myStyles =
    style
        [ ( "backgroundColor", "#111" )
        , ( "color", "#555" )
        , ( "border", "1px solid #777" )
        , ( "margin", " 1rem 20px" )
        ]
