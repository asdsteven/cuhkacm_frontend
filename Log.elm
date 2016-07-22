module Log exposing (..)

import Html exposing (Html)

import Hub
import Port

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
            model ! [Port.notify s]
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
