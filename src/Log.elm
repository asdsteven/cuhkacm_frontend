module Log exposing (..)

import Html exposing (Html)

import Hub
import Ports

type alias Model =
    { log : List String
    }

type Msg
    = HubMsg Hub.Msg
    | Nop


init : Model
init =
    Model []


subscriptions : Sub Msg
subscriptions =
    Sub.none


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        HubMsg (Hub.Error s) ->
            model ! [Ports.notify ("", s, "alert")]
        HubMsg (Hub.Submitted s) ->
            model ! [Ports.notify ("", ("Submitted with sid " ++ s), "default")]
        HubMsg (Hub.Login "") ->
            model ! [Ports.notify ("", "Please enter a team name", "warning")]
        HubMsg (Hub.Notify s) ->
            model ! [Ports.notify s]
        HubMsg (Hub.Oversize _) ->
            model ! [Ports.notify ("", "File exceeds 40KB", "warning")]
        HubMsg _ ->
            model ! []
        Nop ->
            model ! []


view : Model -> Html Msg
view model =
    model.log
        |> List.map Html.text
        |> List.map (\s -> Html.li [] [s])
        |> Html.ul []
        |> (\s -> Html.div [] [s])
