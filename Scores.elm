module Scores exposing (..)

import Array exposing (Array)
import Char
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attr
import String

import Hub

type alias Model =
    { team : Maybe String
    , problems : Array Hub.Problem
    , submissions : Array Hub.Submission
    , scores : Dict String (Array (Int, Maybe Int))
    }

type Msg
    = HubMsg Hub.Msg
    | Nop


init : Model
init =
    Model Nothing Array.empty Array.empty Dict.empty


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
            { model | problems = s } ! []
        HubMsg (Hub.Submissions s) ->
            { model | submissions = s } ! []
        HubMsg (Hub.Scores s) ->
            { model | scores = s } ! []
        HubMsg (Hub.NewSubmission s) ->
            { model | submissions = Array.push s model.submissions } ! []
        HubMsg (Hub.NewTeam s) ->
            let
                row =
                    Array.initialize
                        (Array.length model.problems)
                        (always (0, Nothing))
            in
                { model | scores = Dict.insert s row model.scores } ! []
        HubMsg (Hub.Update (i,v,_)) ->
            let
                g m (s, t) =
                    case t of
                        Just _ ->
                            (s, t)
                        Nothing ->
                            (s + 1, if v == 90 then Just m else Nothing)
                f p m t =
                    case t of
                        Just t' ->
                            Array.get p t'
                                |> Maybe.withDefault (0, Nothing)
                                |> g m
                                |> \x -> Array.set p x t'
                                |> Just
                        Nothing ->
                            Nothing
                s =
                    Array.get i model.submissions
                        |> Maybe.map (\s -> Dict.update s.team (f s.pid s.minutes) model.scores)
                        |> Maybe.map (\s -> { model | scores = s} ! [])
                        |> Maybe.withDefault (model ! [])
            in
                case v of
                    40 -> s
                    45 -> s
                    50 -> s
                    60 -> s
                    70 -> s
                    80 -> s
                    90 -> s
                    _ -> model ! []
        HubMsg _ ->
            model ! []
        Nop ->
            model ! []


view : Model -> Html Msg
view model =
    let
        header =
            List.map Html.text ["Rank", "Team", "Solved", "Penalty"]
        problem c {title, url} =
            Html.a [Attr.href url] [Html.text (String.fromChar c ++ ": " ++ title)]
        az =
            [0..25]
                |> List.map ((+) <| Char.toCode 'A')
                |> List.map Char.fromCode
        problems =
            List.map2 problem az (Array.toList model.problems)
        thead =
            header ++ problems
                |> List.map (\s -> Html.th [] [s])
                |> Html.tr []
                |> (\s -> Html.thead [] [s])
        solved =
            Array.filter ((/=) Nothing << snd)
                >> Array.length
        penalty =
            Array.filter ((/=) Nothing << snd)
                >> Array.map (\(s, t) -> 20 * (s - 1) + Maybe.withDefault 0 t)
                >> Array.foldl (+) 0
        summarize s =
            (solved s, penalty s)
        cmp (_, (s, t), _) (_, (s', t'), _) =
            if s /= s'
            then compare s' s
            else compare t t'
        sorted =
            model.scores
                |> Dict.toList
                |> List.map (\(t, a) -> (t, summarize a, a))
                |> List.sortWith cmp
        shifted =
            List.head sorted
                |> Maybe.map (flip (::) sorted)
                |> Maybe.withDefault []
        differ a b =
            if cmp a b == EQ
            then 0
            else 1
        ranks =
            List.map2 differ sorted shifted
                |> List.scanl (+) 1
                |> List.tail
                |> Maybe.withDefault []
        flatten r (n, (s, t), a) =
            let
                r' =
                    if s == 0
                    then ""
                    else toString r
                display (s, t) =
                    case t of
                        Just t' ->
                            toString s ++ "/" ++ toString t'
                        Nothing ->
                            toString s ++ "/--"
                l =
                    List.map display (Array.toList a)
            in
                r' :: n :: toString s :: toString t :: l
        tbody =
            List.map2 flatten ranks sorted
                |> List.map (List.map (\s -> Html.td [Attr.style [("text-align", "center")]] [Html.text s]))
                |> List.map (Html.tr [])
                |> Html.tbody []
    in
        Html.div
            [ Attr.style
                  [ ("width", "100%")
                  , ("overflow", "auto")
                  ]
            ] [Html.table [Attr.class "table striped border"] [thead, tbody]]
