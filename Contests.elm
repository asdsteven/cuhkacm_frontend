module Contests exposing (..)

import Array exposing (Array)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events

import Hub

type alias Model =
    { contests : Array String
    , contest : String
    }

type Msg
    = HubMsg Hub.Msg
    | Nop


subscriptions : Sub Msg
subscriptions =
    Sub.none


init : Model
init =
    Model Array.empty ""


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        HubMsg (Hub.Contests s) ->
            { model |
                  contests = s,
                  contest = Maybe.withDefault "" (Array.get (Array.length s - 1) s)
            } ! []
        HubMsg (Hub.Contested s _ ) ->
            { model | contest = s } ! []
        HubMsg _ ->
            model ! []
        Nop ->
            model ! []


view : Model -> Html Msg
view model =
    let
        f c =
            Html.button
                [ Attr.classList
                      [ ("button", True)
                      , ("bd-white", True)
                      , ("bottom-shadow", c == model.contest)
                      ]
                , Events.onClick (HubMsg (Hub.Contest c))
                ] [Html.text c]
        g c =
            if c == model.contest
            then Html.option [Attr.value c, Attr.selected True] [Html.text c]
            else Html.option [Attr.value c] [Html.text c]
        buttons =
            Array.map f model.contests
                |> Array.toList
        options =
            Array.map g model.contests
                |> Array.toList
    in
        Html.div [Attr.style [("height", "4em")]]
            [ Html.div
                  [ Attr.class "no-tablet"
                  , Attr.style
                        [ ("overflow-x", "auto")
                        , ("overflow-y", "hidden")
                        , ("white-space", "nowrap")
                        ]
                  ] buttons
            , Html.div
                [ Attr.class "no-pc input-control select"
                , Attr.style [("width", "100%")]
                ]
                [ Html.select
                      [ Events.onInput (HubMsg << Hub.Contest)
                      , Attr.style [("width", "100%")]
                      ] options
                ]
            ]
