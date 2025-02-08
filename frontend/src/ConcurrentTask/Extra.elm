module ConcurrentTask.Extra exposing (attemptEach, toResult)

import ConcurrentTask exposing (ConcurrentTask, Pool, Response, attempt, map, onError, succeed)
import Json.Encode exposing (Value)


attemptEach :
    { pool : Pool msg x a
    , send : Value -> Cmd msg
    , onComplete : Response x a -> msg
    }
    -> List (ConcurrentTask x a)
    -> ( Pool msg x a, List (Cmd msg) )
attemptEach { pool, send, onComplete } taskList =
    List.foldl
        (\task ( poolAccum, cmdAccum ) ->
            attempt
                { pool = poolAccum
                , send = send
                , onComplete = onComplete
                }
                task
                |> Tuple.mapSecond (\nextCmd -> nextCmd :: cmdAccum)
        )
        ( pool, [] )
        taskList


toResult : ConcurrentTask err a -> ConcurrentTask x (Result err a)
toResult task =
    map Ok task
        |> onError (\err -> succeed <| Err err)
