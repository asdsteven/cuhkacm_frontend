module TimePanel exposing (..)

import Date
import Html exposing (Html)
import Html.Attributes as Attr
import Time exposing (Time)

import Hub

type alias Model =
    { server : Maybe Time
    , local : Maybe Time
    , start : Maybe Time
    }

type Msg
    = HubMsg Hub.Msg
    | Local Time


init : Model
init =
    Model Nothing Nothing Nothing


subscriptions : Sub Msg
subscriptions =
    Time.every Time.second Local


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        HubMsg (Hub.Server t) ->
            { model | server = Just t } ! []
        HubMsg (Hub.Start t) ->
            { model | start = Just t } ! []
        HubMsg _ ->
            model ! []
        Local t ->
            let
                last =
                    Maybe.withDefault t model.local
                server =
                    Maybe.map (\s -> s + t - last) model.server
            in
                { model |
                      server = server,
                      local = Just t
                } ! []


view : Model -> Html Msg
view model =
    let
        twoDigits i =
            if 0 <= i && i <= 9
            then "0" ++ toString i
            else toString i
        f (y,mm,d,h,m,s) =
            y ++ "-" ++ mm ++ "-" ++ d ++ " " ++ h ++ ":" ++ m ++ ":" ++ s
        date d = f
            ( toString <| Date.year d
            , toString <| Date.month d
            , toString <| Date.day d
            , twoDigits <| Date.hour d
            , twoDigits <| Date.minute d
            , twoDigits <| Date.second d
            )
        server =
            model.server
                |> Maybe.map Date.fromTime
                |> Maybe.map date
                |> Maybe.withDefault "???"
        start =
            model.start
                |> Maybe.map Date.fromTime
                |> Maybe.map date
                |> Maybe.withDefault "???"
        minutes =
            Maybe.map2 (-) model.server model.start
                |> Maybe.map Time.inMinutes
                |> Maybe.map floor
                |> Maybe.map toString
                |> Maybe.map (flip (++) " minutes")
                |> Maybe.withDefault "???"
    in
        Html.div
            [ Attr.class "flex-grid"
            , Attr.style [("margin", "1em 0")]
            ]
            [ Html.div [Attr.class "row"]
                  [ Html.div
                        [ Attr.class "bd-cyan cell colspan7"
                        , Attr.style
                              [ ("display", "inline-block")
                              , ("text-align", "center")
                              , ("border", "1px solid")
                              , ("border-radius", "10px")
                              , ("margin-top", "1.25em")
                              ]
                        ]
                        [ Html.div
                              [ Attr.style
                                    [ ("display", "inline-block")
                                    , ("width", "90%")
                                    ]
                              ]
                              [ Html.h4
                                    [ Attr.class "fg-cyan place-left no-margin-left"
                                    ] [Html.text "Server Time:"]
                              , Html.h4
                                    [ Attr.class "fg-steel place-right no-margin-right"
                                    ] [Html.text server]
                              ]
                        , Html.div
                              [ Attr.style
                                    [ ("display", "inline-block")
                                    , ("width", "90%")
                                    ]
                              ]
                              [ Html.h4
                                    [ Attr.class "fg-cyan place-left no-margin-left"
                                    ] [Html.text "Contest Start:"]
                              , Html.h4
                                    [ Attr.class "fg-steel place-right no-margin-right"
                                    ] [Html.text start]
                              ]
                        ]
                  , Html.div
                        [ Attr.class "cell colspan5 v-align-middle"
                        , Attr.style [("text-align", "center")]
                        ]
                        [ Html.h3
                              [ Attr.class "fg-cyan v-align-middle no-margin-right"
                              , Attr.style
                                    [ ("display", "inline-block")
                                    , ("margin-top", "1.25em")
                                    ]
                              ] [Html.text "Time:"]
                        , Html.span
                              [ Attr.class "v-align-middle"
                              , Attr.style
                                  [ ("display", "inline-block")
                                  , ("height", "100%")
                                  , ("margin", "0 0.5em")
                                  ]
                              ] []
                        , Html.h3
                              [ Attr.class "fg-steel v-align-middle no-margin-left"
                              , Attr.style
                                    [ ("display", "inline-block")
                                    , ("margin-top", "1.25em")
                                    ]
                              ] [Html.text minutes]
                        ]
                  ]
            ]
