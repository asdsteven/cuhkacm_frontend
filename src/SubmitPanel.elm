module SubmitPanel exposing (..)

import Array
import Char
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode as Decode
import String

import Hub
import Ports

type alias Model =
    { problems : List String
    , pid : String
    , language : String
    , fileReading : Bool
    , submitting : Bool
    , event : Maybe Decode.Value
    }

type Msg
    = HubMsg Hub.Msg
    | Pid String
    | Language String
    | FileChange Decode.Value
    | Submit


init : Model
init =
    Model [] "0" "c" False False Nothing


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.fileRead (HubMsg << Hub.Submit model.pid model.language)
        , Ports.oversize (HubMsg << Hub.SubmitOversize)
        ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        HubMsg (Hub.Submitted _) ->
            { model | submitting = False } ! []
        HubMsg (Hub.SubmitOversize _) ->
            { model | submitting = False } ! []
        HubMsg (Hub.Problems s) ->
            { model | problems = List.map .title (Array.toList s) } ! []
        HubMsg _ ->
            model ! []
        Pid i ->
            { model | pid = i } ! []
        Language s ->
            { model | language = s } ! []
        FileChange event ->
            { model | event = Just event } ! []
        Submit ->
            let
                cmd =
                    model.event
                        |> Maybe.map (\s -> [Ports.readFile s])
                        |> Maybe.withDefault []
            in
                { model | submitting = True } ! cmd


view : Model -> Html Msg
view model =
    let
        onChange =
            Ok << FileChange
                |> Decode.customDecoder Decode.value
                |> Events.on "change"
        onSelect f =
            Events.on "change" (Decode.map f Events.targetValue)
        option a b =
            Html.option [Attr.value a] [Html.text b]
        az =
            [0..25]
                |> List.map ((+) <| Char.toCode 'A')
                |> List.map Char.fromCode
        problems =
            model.problems
                |> List.map2 (\a b -> String.cons a ": " ++ b) az
                |> List.map2 option (List.map toString [0..25])
        languages =
            List.map2 option
                ["c", "c++", "c++11", "java"]
                ["C", "C++", "C++ 11", "Java"]
        selectProblem =
            Html.div
                [ Attr.class "input-control full-size no-margin" ]
                [ Html.select [onSelect Pid] problems ]
        selectLanguage =
            Html.div
                [ Attr.class "input-control full-size no-margin" ]
                [ Html.select [onSelect Language] languages ]
        upload =
            Html.div
                [ Attr.class "input-control file full-size no-margin"
                , Attr.attribute "data-role" "input"
                ]
                [ Html.input [Attr.type' "file", onChange] []
                , Html.button
                      [ Attr.class "button" ]
                      [ Html.span [Attr.class "mif-folder fg-cyan"] [] ]
                ]
        submit =
            Html.button
                [ Attr.type' "submit"
                , Attr.classList
                      [ ("button full-size no-margin", True)
                      , ("loading-cube", model.submitting)
                      ]
                , Attr.disabled model.submitting
                ]
                [ Html.text "Submit" ]
        onSubmit =
            case model.event of
                Just _ ->
                    Submit
                Nothing ->
                    HubMsg Hub.SubmitNoFile
    in
        Html.form
            [ Events.onSubmit onSubmit ]
            [ Html.div [Attr.class "margin20 no-phone"] []
            , Html.div
                  [ Attr.class "flex-grid" ]
                  [ Html.div
                        [ Attr.class "row" ]
                        [ Html.div
                              [ Attr.class "cell colspan3 margin10 no-margin-top no-margin-left no-margin-right"]
                              [ selectProblem ]
                        , Html.div
                              [ Attr.class "cell colspan2 margin10 no-margin-top no-margin-left no-margin-right"]
                              [ selectLanguage ]
                        , Html.div
                              [ Attr.class "cell colspan5 margin10 no-margin-top no-margin-left no-margin-right"]
                              [ upload ]
                        , Html.div
                              [ Attr.class "cell colspan2 margin10 no-margin-top no-margin-left no-margin-right"]
                              [ submit ]
                        ]
                  ]
            ]
