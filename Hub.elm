module Hub exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Time exposing (Time)

type alias Problem =
    { title : String
    , url : String
    }

type alias Submission =
    { team : String
    , pid : Int
    , language : String
    , verdict : Int
    , runtime : Maybe Int
    , minutes : Int
    , url : String
    }

type Msg
    = Error String
    | Contests (Array String)
    | Server Time
    | Contest String
    | Contested String (Maybe String)
    | Start Time
    | Problems (Array Problem)
    | Submissions (Array Submission)
    | Scores (Dict String (Array (Int, Maybe Int)))
    | Login String
    | Logined String
    | Logout
    | Logouted
    | Submit String String String
    | Submitted String
    | NewSubmission Submission
    | NewTeam String
    | Update (Int, Int, Maybe Int)
