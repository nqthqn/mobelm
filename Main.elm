module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes as Attr exposing (..)


type alias Model =
    { utc : Float
    , times : List LocalTime
    }


type alias LocalTime =
    { name : String
    , offset : Float
    }



-- define contructors of Msg
-- define an UpdateUTC that takes a string


type Msg
    = UpdateUTC String



-- tagged type,


makeUpdate : String -> Msg
makeUpdate =
    UpdateUTC



-- pass in the necessary parameters to return a concrete type


initModel : Model
initModel =
    { utc = 17
    , times = initLocalTimes
    }


initLocalTimes : List LocalTime
initLocalTimes =
    [ { name = "EDT", offset = -4 }
    , { name = "PDT", offset = -7 }
    , { name = "IST", offset = 5.5 }
    ]


localTimeView : Float -> LocalTime -> Html Msg
localTimeView utc t =
    li [] [ text <| "Time zone: " ++ t.name ++ "  " ++ toString (utc + t.offset) ]


localTimeList : Model -> Html Msg
localTimeList model =
    ul [] (List.map (localTimeView model.utc) model.times)


view : Model -> Html Msg
view model =
    div [ id "root" ]
        [ h1 [] [ text "mobelm" ]
        , h4 [] [ text "time picking" ]
        , input
            [ type_ "range"
            , Attr.min "0"
            , Attr.max "24"
            , step ".5"
            , onInput makeUpdate
            ]
            []
        , text (toString model.utc)
        , localTimeList model
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateUTC utc ->
            case String.toFloat utc of
                Ok utc ->
                    { model | utc = utc }

                Err _ ->
                    Debug.crash "Something went horribly wrong"


main : Program Never Model Msg
main =
    beginnerProgram { model = initModel, view = view, update = update }
