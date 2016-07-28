module Main exposing (..)

import Array
import Html exposing (Html)
import Html.App as App
import Html.Attributes as Attr

import Contests
import Greeting
import TimePanel
import LoginPanel
import SubmitPanel
import Submissions
import Scores
import Log

import Hub
import Ws

type alias Model =
    { ws: Ws.Model
    , contests : Contests.Model
    , greeting : Greeting.Model
    , timePanel : TimePanel.Model
    , loginPanel : LoginPanel.Model
    , submitPanel : SubmitPanel.Model
    , submissions : Submissions.Model
    , scores : Scores.Model
    , log : Log.Model
    }

type Msg
    = HubMsg Hub.Msg
    | WsMsg Ws.Msg
    | ContestsMsg Contests.Msg
    | GreetingMsg Greeting.Msg
    | TimePanelMsg TimePanel.Msg
    | LoginPanelMsg LoginPanel.Msg
    | SubmitPanelMsg SubmitPanel.Msg
    | SubmissionsMsg Submissions.Msg
    | ScoresMsg Scores.Msg
    | LogMsg Log.Msg


main : Program Never
main =
    App.program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


init : (Model, Cmd Msg)
init =
    { ws = Ws.init
    , contests = Contests.init
    , greeting = Greeting.init
    , timePanel = TimePanel.init
    , loginPanel = LoginPanel.init
    , submitPanel = SubmitPanel.init
    , submissions = Submissions.init
    , scores = Scores.init
    , log = Log.init
    } ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map WsMsg Ws.subscriptions
        , Sub.map ContestsMsg Contests.subscriptions
        , Sub.map GreetingMsg Greeting.subscriptions
        , Sub.map TimePanelMsg TimePanel.subscriptions
        , Sub.map LoginPanelMsg LoginPanel.subscriptions
        , Sub.map SubmitPanelMsg <| SubmitPanel.subscriptions model.submitPanel
        , Sub.map SubmissionsMsg Submissions.subscriptions
        , Sub.map ScoresMsg Scores.subscriptions
        , Sub.map LogMsg Log.subscriptions
        ]


hubUpdate : Hub.Msg -> Model -> (Model, Cmd Msg)
hubUpdate msg model =
    let
        t f g h i u (model, cmd) =
            let (model', cmd') = u (f msg) (i model)
            in h model model' ! [cmd,Cmd.map g cmd']
    in
        model ! []
            |> t Ws.HubMsg WsMsg (\m s -> { m | ws = s }) .ws Ws.update
            |> t Contests.HubMsg ContestsMsg (\m s -> { m | contests = s }) .contests Contests.update
            |> t Greeting.HubMsg GreetingMsg (\m s -> { m | greeting = s }) .greeting Greeting.update
            |> t TimePanel.HubMsg TimePanelMsg (\m s -> { m | timePanel = s }) .timePanel TimePanel.update
            |> t LoginPanel.HubMsg LoginPanelMsg (\m s -> { m | loginPanel = s }) .loginPanel LoginPanel.update
            |> t SubmitPanel.HubMsg SubmitPanelMsg (\m s -> { m | submitPanel = s }) .submitPanel SubmitPanel.update
            |> t Submissions.HubMsg SubmissionsMsg (\m s -> { m | submissions = s }) .submissions Submissions.update
            |> t Scores.HubMsg ScoresMsg (\m s -> { m | scores = s }) .scores Scores.update
            |> t Log.HubMsg LogMsg (\m s -> { m | log = s }) .log Log.update


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let
        t f g u s m =
            let (m', c') = u s m
            in (g model m', Cmd.map f c')
    in
        case msg of
            HubMsg s ->
                hubUpdate s model
            WsMsg (Ws.HubMsg s) ->
                hubUpdate s model
            WsMsg (Ws.Responded i s) ->
                hubUpdate s { model | ws = Ws.responded i model.ws }
            ContestsMsg (Contests.HubMsg s) ->
                hubUpdate s model
            GreetingMsg (Greeting.HubMsg s) ->
                hubUpdate s model
            TimePanelMsg (TimePanel.HubMsg s) ->
                hubUpdate s model
            LoginPanelMsg (LoginPanel.HubMsg s) ->
                hubUpdate s model
            SubmitPanelMsg (SubmitPanel.HubMsg s) ->
                hubUpdate s model
            SubmissionsMsg (Submissions.HubMsg s) ->
                hubUpdate s model
            ScoresMsg (Scores.HubMsg s) ->
                hubUpdate s model
            LogMsg (Log.HubMsg s) ->
                hubUpdate s model
            WsMsg s ->
                t WsMsg (\m s -> { m | ws = s }) Ws.update s model.ws
            ContestsMsg s ->
                t ContestsMsg (\m s -> { m | contests = s }) Contests.update s model.contests
            GreetingMsg s ->
                t GreetingMsg (\m s -> { m | greeting = s }) Greeting.update s model.greeting
            TimePanelMsg s ->
                t TimePanelMsg (\m s -> { m | timePanel = s }) TimePanel.update s model.timePanel
            LoginPanelMsg s ->
                t LoginPanelMsg (\m s -> { m | loginPanel = s }) LoginPanel.update s model.loginPanel
            SubmitPanelMsg s ->
                t SubmitPanelMsg (\m s -> { m | submitPanel = s }) SubmitPanel.update s model.submitPanel
            SubmissionsMsg s ->
                t SubmissionsMsg (\m s -> { m | submissions = s }) Submissions.update s model.submissions
            ScoresMsg s ->
                t ScoresMsg (\m s -> { m | scores = s }) Scores.update s model.scores
            LogMsg s ->
                t LogMsg (\m s -> { m | log = s }) Log.update s model.log


view : Model -> Html Msg
view model =
    let
        contests =
            App.map ContestsMsg (Contests.view model.contests)
        greeting =
            App.map GreetingMsg (Greeting.view model.greeting)
        timePanel =
            App.map TimePanelMsg (TimePanel.view model.timePanel)
        loginPanel =
            App.map LoginPanelMsg (LoginPanel.view model.loginPanel)
        submitPanel =
            App.map SubmitPanelMsg (SubmitPanel.view model.submitPanel)
        submissions =
            App.map SubmissionsMsg (Submissions.view model.submissions)
        scores =
            App.map ScoresMsg (Scores.view model.scores)
        log =
            App.map LogMsg (Log.view model.log)
        loading =
            Html.div
                [ Attr.class "leader align-center"
                , Attr.style [("line-height", "40vh")]
                ]
                [ Html.span
                      [ Attr.class "mif-spinner3 mif-ani-spin fg-blue mif-4x"
                      , Attr.style [("vertical-align", "-25%")]
                      ] []
                , Html.text " Loading"
                ]
        page =
            Html.div
                [ Attr.class "container" ]
                [ contests
                , greeting
                , timePanel
                , if model.greeting.team == Nothing
                  then loginPanel
                  else submitPanel
                , submissions
                , scores
                , log
                ]
    in
        if Array.length (model.contests.contests) == 0
        then loading
        else page
