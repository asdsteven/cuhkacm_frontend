module SubmitPanel exposing (..)

import Array
import Char
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode as Decode
import String

import Hub
import Port

type alias Model =
    { problems : List String
    , pid : String
    , language : String
    , fileReading : Bool
    , file : Maybe (String, Int, String)
    , submitting : Bool
    }

type Msg
    = HubMsg Hub.Msg
    | Pid String
    | Language String
    | FileChange Decode.Value
    | FileRead (String, Int, String)


init : Model
init =
    Model [] "0" "c" False Nothing False


subscriptions : Sub Msg
subscriptions =
    Port.fileRead FileRead


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        HubMsg (Hub.Submit _ _ _) ->
            { model | submitting = True } ! []
        HubMsg (Hub.Submitted _) ->
            { model | submitting = False, file = Nothing } ! []
        HubMsg (Hub.Problems s) ->
            { model | problems = List.map .title (Array.toList s) } ! []
        HubMsg _ ->
            model ! []
        Pid i ->
            { model | pid = i} ! []
        Language s ->
            { model | language = s} ! []
        FileChange event ->
            { model | fileReading = True, file = Nothing } ! [Port.fileChange event]
        FileRead f ->
            { model | fileReading = False, file = Just f } ! []


view : Model -> Html Msg
view model =
    let
        onChange =
            Ok << FileChange
                |> Decode.customDecoder Decode.value
                |> Events.on "change"
        oversized =
            model.file
                |> Maybe.map (\(_,s,_) -> s > 40000)
                |> Maybe.withDefault False
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
        f (name,size,_) =
            if oversized
            then name ++ " (" ++ toString size ++ " bytes, exceeded 40KB)"
            else name ++ " (" ++ toString size ++ " bytes)"
        fileText =
            if model.fileReading
            then
                "Reading..."
            else
                model.file
                    |> Maybe.map f
                    |> Maybe.withDefault ""
        fileColor =
            if model.fileReading
            then "black"
            else if oversized
                 then "crimson"
                 else "green"
        submitDisabled =
            oversized || model.submitting || model.file == Nothing
        submitHtml =
            if oversized
            then [Html.text "> 40KB"]
            else [Html.text "Submit"]
        controls =
            [ Html.div
                  [ Attr.class "input-control select"
                  , Attr.style [("width", "100%")]
                  ]
                  [ Html.select
                        [ Events.onInput Pid
                        ] problems
                  ]
            , Html.div
                  [ Attr.class "input-control select"
                  , Attr.style [("width", "100%")]
                  ]
                  [ Html.select
                        [ Events.onInput Language
                        ] languages
                  ]
            , Html.div
                  [ Attr.class "input-control file"
                  , Attr.attribute "data-role" "input"
                  , Attr.style [("width", "100%")]
                  ]
                  [ Html.input [Attr.type' "file", onChange] []
                  , Html.button [Attr.class "button"]
                        [ Html.span [Attr.class "mif-folder fg-cyan"] []
                        ]
                  ]
            , Html.div
                  [ Attr.class "input-control"
                  , Attr.style [("width", "100%")]
                  ]
                  [ Html.button
                        [ Attr.classList
                              [ ("button", True)
                              , ("fg-white", not submitDisabled)
                              , ("bg-cyan", not submitDisabled)
                              , ("fg-darkRed", oversized)
                              , ("loading-cube", model.submitting)
                              ]
                        , Attr.style
                            [ ("width", "100%")
                            , ("margin", "0")
                            ]
                        , Attr.type' "submit"
                        , Attr.disabled submitDisabled
                        ] submitHtml
                  ]
            ]
        grids = List.map (\s -> Html.div [Attr.class s])
            [ "cell colspan3"
            , "cell colspan2"
            , "cell colspan5"
            , "cell colspan2"
            ]
        gridded =
            List.map2 (\g c -> g [c]) grids controls
        onSubmit =
            model.file
                |> Maybe.map (\(_,_,s) -> Hub.Submit model.pid model.language s)
                |> Maybe.withDefault (Hub.Error "no file")
                |> HubMsg
    in
        Html.form
            [ Attr.class "submitPanel"
            , Events.onSubmit onSubmit
            ]
            [ Html.div [Attr.class "grid condensed"]
                  [ Html.div [Attr.class "row cells12"] gridded
                  ]
            ]
