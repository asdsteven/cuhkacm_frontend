module Notify exposing (..)

import Hub
import Ports


update : Hub.Msg -> Cmd msg
update msg =
    case msg of
        Hub.Error s ->
            Ports.notify ("", s, "alert")
        Hub.LoginNoName ->
            Ports.notify ("", "Please enter a team name", "warning")
        Hub.Submitted s ->
            Ports.notify ("", "Submitted with sid " ++ s, "default")
        Hub.SubmitNoFile ->
            Ports.notify ("", "Please upload a file", "warning")
        Hub.SubmitOversize _ ->
            Ports.notify ("", "File exceeds 40KB", "warning")
        _ ->
            Cmd.none
