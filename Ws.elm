module Ws exposing (..)

import Debug
import Dict exposing (Dict)
import Json.Decode as Decode
import String
import WebSocket

import Hub

type alias Model =
    { rid : Int
    , queue : Dict Int String
    }

type Msg
    = HubMsg Hub.Msg
    | Responded Int Hub.Msg
    | Handshake String


init : Model
init =
    Model 0 Dict.empty


url : String
url = "ws://cuhkacm.herokuapp.com/ws"
--url = "ws://localhost:5000/ws"


send : String -> Cmd msg
send =
    WebSocket.send url


sendRequest : Int -> String -> Cmd msg
sendRequest rid s =
    send ("request " ++ toString rid ++ " " ++ s)


listen : (String -> msg) -> Sub msg
listen f =
    WebSocket.listen url (\s -> Debug.log "listen" s |> f)


request : String -> Model -> (Model, Cmd Msg)
request s model =
    { model |
          rid = model.rid + 1,
          queue = Dict.insert model.rid s model.queue
    } ! [sendRequest model.rid s]


responded : Int -> Model -> Model
responded rid model =
    { model | queue = Dict.remove rid model.queue }


subscriptions : Sub Msg
subscriptions =
    listen parse


parse : String -> Msg
parse msg =
    let
        nextToken s =
            case String.uncons s of
                Just (' ', t) -> ("", t)
                Just (h, t) ->
                    let (h', t') = nextToken t
                    in (String.cons h h', t')
                Nothing -> ("", "")
        maybe = Decode.maybe
        int = Decode.int
        str = Decode.string
        handshake =
            Handshake >> Ok
        server =
            String.toFloat
                >> Result.map (Hub.Server >> HubMsg)
        contests s =
            Decode.array str
                |> flip Decode.decodeString s
                |> Result.map (Hub.Contests >> HubMsg)
        start =
            String.toFloat
                >> Result.map (Hub.Start >> HubMsg)
        problems s =
            Decode.tuple4 (\a b c d -> Hub.Problem a b) str str str str
                |> Decode.array
                |> flip Decode.decodeString s
                |> Result.map (Hub.Problems >> HubMsg)
        submissions s =
            Decode.tuple7 Hub.Submission str int str int (maybe int) int str
                |> Decode.array
                |> flip Decode.decodeString s
                |> Result.map (Hub.Submissions >> HubMsg)
        scores s =
            Decode.tuple2 (,) int (maybe int)
                |> Decode.array
                |> Decode.dict
                |> flip Decode.decodeString s
                |> Result.map (Hub.Scores >> HubMsg)
        respond s =
            let
                handle (res, s) =
                    case res of
                        "contest" ->
                            Decode.tuple2 Hub.Contested str (maybe str)
                                |> flip Decode.decodeString s
                        "contestErr" -> Ok (Hub.Error ("contestErr" ++ s))
                        "login" -> Ok (Hub.Logined s)
                        "logout" -> Ok Hub.Logouted
                        "submit" -> Ok (Hub.Submitted s)
                        "submitErr" -> Ok (Hub.Error ("submitErr" ++ s))
                        _ -> Err ("invalid response: " ++ res)
                (id, s') = nextToken s
            in
                Result.map2 Responded (String.toInt id) (handle (nextToken s'))
        newSubmission s =
            Decode.tuple7 Hub.Submission str int str int (maybe int) int str
                |> flip Decode.decodeString s
                |> Result.map (Hub.NewSubmission >> HubMsg)
        newTeam =
            Hub.NewTeam >> HubMsg >> Ok
        update s =
            Decode.tuple3 (,,) int int (maybe int)
                |> flip Decode.decodeString s
                |> Result.map (Hub.Update >> HubMsg)
        handle (h, s) =
            case h of
                "handshake" -> handshake s
                "server" -> server s
                "contests" -> contests s
                "start" -> start s
                "problems" -> problems s
                "submissions" -> submissions s
                "scores" -> scores s
                "respond" -> respond s
                "newSubmission" -> newSubmission s
                "newTeam" -> newTeam s
                "update" -> update s
                _ -> Err ("invalid handler: " ++ h)
    in
        case handle (nextToken msg) of
            Ok x -> x
            Err err -> HubMsg (Hub.Error ("wsparse: " ++ msg ++ ": " ++ err))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        HubMsg (Hub.Contest s) ->
            request ("contest " ++ s) model
        HubMsg (Hub.Login s) ->
            request ("login " ++ s) model
        HubMsg (Hub.Logout) ->
            request "logout" model
        HubMsg (Hub.Submit p l s) ->
            request ("submit " ++ p ++ " " ++ l ++ " " ++ s) model
        HubMsg _ ->
            model ! []
        Responded _ _ ->
            model ! []
        Handshake s ->
            model.queue
                |> Dict.toList
                |> List.map (\(rid,s) -> sendRequest rid s)
                |> (::) (send ("handshake " ++ s))
                |> (!) model
