module Greeting exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events

import Hub

type alias Model =
    { team : Maybe String
    , logouting : Bool
    }

type Msg
    = HubMsg Hub.Msg
    | Nop


init : Model
init =
    Model Nothing False


subscriptions : Sub Msg
subscriptions =
    Sub.none


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        HubMsg (Hub.Logined s) ->
            { model | team = Just s } ! []
        HubMsg (Hub.Logout) ->
            { model | logouting = True } ! []
        HubMsg (Hub.Logouted) ->
            { model | team = Nothing, logouting = False } ! []
        HubMsg (Hub.Contested _ t) ->
            { model | team = t } ! []
        HubMsg _ ->
            model ! []
        Nop ->
            model ! []


view : Model -> Html Msg
view model =
    let
        helloGuest =
            Html.h2 []
                [ Html.small [] [Html.text "Hello, "]
                , Html.text "Guest"
                ]
        hello team =
            Html.h2 []
                [ Html.small [] [Html.text "Hello, "]
                , Html.text team
                , Html.button
                    [ Attr.classList
                          [ ("button", True)
                          , ("rounded", True)
                          , ("small-button", True)
                          , ("fg-darkRed", not model.logouting)
                          , ("bd-darkRed", not model.logouting)
                          , ("loading-cube", model.logouting)
                          ]
                    , Attr.style [("margin-left", "1em")]
                    , Attr.disabled model.logouting
                    , Events.onClick (HubMsg Hub.Logout)
                    ] [Html.text "Logout"]
                ]
    in
        case model.team of
            Just team ->
                hello team
            Nothing ->
                helloGuest
