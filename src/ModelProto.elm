module Main exposing (Model, Params, Path, Point, Vector)

import Dict


type alias Params =
    Dict.Dict String Float


type alias Vector =
    { x : Float, y : Float }


type alias Trajectory =
    { time : Float, velocity : Vector }


type alias Point =
    { position : Vector, trajectory : Maybe Trajectory }


type alias WayPoint =
    { x : Float
    , y : Float
    , angle : Float
    }


type alias Robot =
    Params


type alias Path =
    { points : List WayPoint
    , pathParams : Params
    , trajectoryParams : Params
    , path : List Point
    }


type CalculationStatus
    = Loading
    | Failed
    | Success


type alias Status =
    { path : CalculationStatus, trajectory : CalculationStatus }


type alias AutoPaths =
    List Path


type TabsState
    = PathTab
    | TrajectoryTab


type alias PathTabsState =
    Int


type alias State =
    { pathIndex : Int
    , zoom : Float
    , pan : Vector
    , paramsTabState : TabsState
    , pathTabState : PathTabsState
    }


type alias Model =
    { pathHistory : List AutoPaths
    , state : State
    , status : Status
    }
