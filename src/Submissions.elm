module Submissions exposing (..)

import Array exposing (Array)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events

import Hub

type alias Model =
    { team : Maybe String
    , problems : Array String
    , submissions : Array Hub.Submission
    , showAcceptedOnly : Bool
    , showMineOnly : Bool
    }

type Msg
    = HubMsg Hub.Msg
    | ShowAcceptedOnly Bool
    | ShowMineOnly Bool


init : Model
init =
    Model Nothing Array.empty Array.empty False False


subscriptions : Sub Msg
subscriptions =
    Sub.none


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        HubMsg (Hub.Logined s) ->
            { model | team = Just s } ! []
        HubMsg (Hub.Logouted) ->
            { model | team = Nothing } ! []
        HubMsg (Hub.Contested _ t) ->
            { model | team = t } ! []
        HubMsg (Hub.Problems s) ->
            { model | problems = Array.map .title s } ! []
        HubMsg (Hub.Submissions s) ->
            { model | submissions = s } ! []
        HubMsg (Hub.NewSubmission s) ->
            { model | submissions = Array.push s model.submissions } ! []
        HubMsg (Hub.Update (i,v,r)) ->
            Array.get i model.submissions
                |> Maybe.map (\s -> { s | verdict = v, runtime = r })
                |> Maybe.map (\s -> Array.set i s model.submissions)
                |> Maybe.map (\s -> { model | submissions = s })
                |> Maybe.withDefault model
                |> flip (!) []
        HubMsg _ ->
            model ! []
        ShowAcceptedOnly t ->
            { model | showAcceptedOnly = t } ! []
        ShowMineOnly t ->
            { model | showMineOnly = t } ! []


view : Model -> Html Msg
view model =
    let
        header =
            [ "sid"
            , "Team"
            , "Problem"
            , "Language"
            , "Verdict"
            , "Runtime"
            , "Time"
            ]
        thead =
            header
                |> List.map (\s -> Html.th [] [Html.text s])
                |> Html.tr []
                |> (\s -> Html.thead [] [s])
        verdictToHtml v =
            let
                judging =
                    case v of
                        0 -> True
                        20 -> True
                        _ -> False
                s = verdictToString v |> Html.text
                spinner =
                    Html.span [Attr.class "mif-spinner3 mif-ani-spin fg-blue"] []
            in
                if judging
                then Html.span [] [spinner, Html.text " ", s]
                else s
        verdictToString v =
            case v of
                0 -> "Submitting"
                10 -> "Submission error"
                15 -> "Can't be judged"
                20 -> "In queue"
                30 -> "Compile error"
                35 -> "Restricted function"
                40 -> "Runtime error"
                45 -> "Output limit exceeded"
                50 -> "Time limit exceeded"
                60 -> "Memory limit exceeded"
                70 -> "Wrong answer"
                80 -> "Presentation error"
                90 -> "Accepted"
                100 -> "System Error"
                101 -> "Validation Error"
                _ -> "???"

        runtimeToString r =
            case r of
                Just r' -> toString r' ++ " ms"
                Nothing -> ""
        row (i, s) =
            [ Html.a [Attr.href s.url] [toString i |> Html.text]
            , s.team |> Html.text
            , Array.get s.pid model.problems |> Maybe.withDefault "???" |> Html.text
            , s.language |> Html.text
            , verdictToHtml s.verdict
            , runtimeToString s.runtime |> Html.text
            , toString s.minutes |> Html.text
            ]
        filter (_, s) =
            (not model.showAcceptedOnly || s.verdict == 90) && (not model.showMineOnly || Just s.team == model.team)
        tbody =
            Array.toList model.submissions
                |> List.indexedMap (,)
                |> List.filter filter
                |> List.map row
                |> List.reverse
                |> List.map (List.map (\s -> Html.td [] [s]))
                |> List.map (Html.tr [])
                |> Html.tbody []
    in
        Html.div
            [ Attr.class "padding10 no-padding-top no-padding-bottom margin10 no-margin-left no-margin-right"
            , Attr.style
                  [ ("height", "12em")
                  , ("overflow", "auto")
                  , ("box-shadow", "inset 0px 0px 3px 0px rgba(0, 0, 0, 0.3), inset 0px 4px 5px 0px rgba(0, 0, 0, 0.2)")
                  ]
            ]
            [ Html.div
                  [ Attr.class "margin10 no-margin-left no-margin-right"]
                  [ Html.label
                        [ Attr.class "input-control checkbox small-check margin10 no-margin-top no-margin-bottom no-margin-left" ]
                        [ Html.input
                              [ Attr.type' "checkbox"
                              , Events.onCheck ShowAcceptedOnly
                              ] []
                        , Html.span [Attr.class "check"] []
                        , Html.span
                              [ Attr.class "caption" ]
                              [ Html.text "Show Accepted only" ]
                        ]
                  , Html.label
                        [ Attr.class "input-control checkbox small-check no-margin" ]
                        [ Html.input
                              [ Attr.type' "checkbox"
                              , Attr.disabled (model.team == Nothing)
                              , Events.onCheck ShowMineOnly
                              ] []
                        , Html.span [Attr.class "check"] []
                        , Html.span
                              [ Attr.class "caption" ]
                              [ Html.text "Show mine only" ]
                        ]
                  ]
            , Html.table
                  [ Attr.class "table striped border no-margin-top" ]
                  [ thead, tbody ]
            ]
