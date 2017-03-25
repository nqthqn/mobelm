port module MobTime exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (..)


type alias Model =
    { seconds : Float
    , mobbers : List String
    , current : String
    , codingSeconds : Float
    , errors : String
    , mobber : String
    , mobbing : Bool
    }


initModel : Model
initModel =
    { seconds = 0
    , mobbers = []
    , current = "Ready to mob?"
    , codingSeconds = 300
    , errors = ""
    , mobber = ""
    , mobbing = False
    }


takeDrop : List a -> List a
takeDrop list =
    let
        head =
            List.take 1 list

        tail =
            List.drop 1 list
    in
        tail ++ head


timesUp : Float -> Float -> Bool
timesUp curTime limit =
    if curTime < limit then
        False
    else
        True


type Msg
    = Tick Time
    | UpdateCodingSeconds String
    | UpdateMobber String
    | AddMobber
    | RemoveMobber String
    | Reset
    | StartMobbing
    | Skip


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Skip ->
            let
                seconds_ =
                    model.codingSeconds
            in
                ( { model | seconds = seconds_ }, Cmd.none )

        StartMobbing ->
            let
                toggledMobbing =
                    not model.mobbing

                reset =
                    model.codingSeconds
            in
                ( { model | mobbing = toggledMobbing, seconds = reset }, Cmd.none )

        Reset ->
            ( initModel, Cmd.none )

        AddMobber ->
            case model.mobber of
                "" ->
                    model ! []

                _ ->
                    let
                        mobbers_ =
                            model.mobber :: model.mobbers
                    in
                        ( { model | mobbers = mobbers_, mobber = "" }, Cmd.none )

        RemoveMobber name ->
            ( { model | mobbers = List.filter (\mob -> mob /= name) model.mobbers }, Cmd.none )

        UpdateMobber mobber_ ->
            ( { model | mobber = mobber_ }, Cmd.none )

        UpdateCodingSeconds num ->
            case String.toFloat num of
                Ok num ->
                    ( { model | codingSeconds = num }, Cmd.none )

                _ ->
                    model ! []

        Tick t ->
            let
                timeIsUp =
                    timesUp model.seconds model.codingSeconds

                seconds_ =
                    if timeIsUp then
                        0
                    else if model.mobbing then
                        model.seconds + 1
                    else
                        model.seconds

                fst =
                    case List.head model.mobbers of
                        Nothing ->
                            "No mobbers!"

                        Just aMobber ->
                            aMobber

                current_ =
                    if timeIsUp then
                        fst
                    else
                        model.current

                mobbers_ =
                    if timeIsUp then
                        takeDrop model.mobbers
                    else
                        model.mobbers

                ping =
                    if timeIsUp then
                        pinger model
                    else
                        Cmd.none
            in
                ( { model
                    | seconds = seconds_
                    , current = current_
                    , mobbers = mobbers_
                  }
                , ping
                )


mobbersList mobbers =
    List.map (\m -> li [] [ text m, button [ class "delete", onClick (RemoveMobber m) ] [ text "X" ] ]) mobbers


timerString model =
    toString (model.codingSeconds - model.seconds)


isMobbing model =
    case model.mobbing of
        True ->
            p []
                [ a [ onClick Skip, class "skip" ] [ text "Skip" ]
                , br [] []
                , text <| (timerString model) ++ " seconds to go."
                ]

        False ->
            button [ class "white", onClick StartMobbing ] [ text "START MOB" ]


activeMobbing model =
    case (List.isEmpty model.mobbers) of
        True ->
            div [] []

        False ->
            div [ class "godfather pad" ]
                [ isMobbing model
                , ul [] (mobbersList model.mobbers)
                ]


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ h1 [] [ text <| model.current ]
        , activeMobbing model
        , span [] [ text "NAME" ]
        , input [ type_ "text", placeholder "Suzy", onInput UpdateMobber, value model.mobber ] []
        , button [ class "black", onClick AddMobber ] [ text "ADD MOBBER" ]
        , br [] []
        , br [] []
        , span [] [ text "SET THE TIME (in seconds)" ]
        , input [ type_ "text", placeholder "300", onInput UpdateCodingSeconds, value (toString model.codingSeconds) ] []
        , span [] [ text <| "= " ++ (toString (model.codingSeconds / 60)) ++ " minutes each" ]
        , button [ class "black", onClick Reset ] [ text "RESET ALL OF THIS" ]
        , div [ class "godfather" ] [ img [ src "http://files.vividscreen.info/soft/b64f2c51a245c9e7dc6ba2d65860642f/The-Godfather-Don-Vito-square-l.jpg" ] [] ]
        ]


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


subs : Model -> Sub Msg
subs model =
    Sub.batch [ every second Tick ]


port pinger : Model -> Cmd msg



-- what is the Never for?


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subs
        }
