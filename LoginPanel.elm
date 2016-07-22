module LoginPanel exposing (..)

import Dict
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Set exposing (Set)

import Hub

type alias Model =
    { name : String
    , logining : Bool
    , teams : Set String
    }

type Msg
    = HubMsg Hub.Msg
    | Name String


init : Model
init =
    Model "" False Set.empty


subscriptions : Sub Msg
subscriptions =
    Sub.none


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        HubMsg (Hub.Login _) ->
            { model | logining = True } ! []
        HubMsg (Hub.Logined _) ->
            { model | logining = False, name = "" } ! []
        HubMsg (Hub.Scores s) ->
            { model | teams = Set.fromList (Dict.keys s) } ! []
        HubMsg (Hub.NewTeam t) ->
            { model | teams = Set.insert t model.teams } ! []
        HubMsg _ ->
            model ! []
        Name s ->
            { model | name = s } ! []


view : Model -> Html Msg
view model =
    let
        teamExists =
            model.teams
                |> Set.member model.name
        buttonText =
            if teamExists
            then "Login"
            else "New team"
        buttonDisabled =
            model.logining || model.name == ""
    in
        Html.form [Events.onSubmit (HubMsg (Hub.Login model.name))]
            [ Html.div
                  [ Attr.class "input-control text"
                  , Attr.style [("width", "100%")]
                  ]
                  [ Html.input
                        [ Attr.type' "text"
                        , Attr.placeholder "Team Name"
                        , Events.onInput Name
                        ] []
                  , Html.button
                      [ Attr.classList
                            [ ("button", True)
                            , ("fg-white", not buttonDisabled)
                            , ("bg-green", teamExists && not buttonDisabled)
                            , ("bg-cyan", not teamExists && not buttonDisabled)
                            , ("loading-cube", model.logining)
                            ]
                      , Attr.type' "submit"
                      , Attr.disabled buttonDisabled
                      ] [Html.text buttonText]
                  ]
            ]
