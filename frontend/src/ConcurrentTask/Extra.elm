module ConcurrentTask.Extra exposing (attemptEach, toResult)

{-| Helper module adding few functions to the ConcurrentTask module.
-}

import ConcurrentTask exposing (ConcurrentTask, Pool, Response, attempt, map, onError, succeed)
import Json.Encode exposing (Value)


{-| Start multiple `ConcurrentTask`s concurrently.

You would use `attemptEach` instead of `attempt` after `batch`
when you do not want to wait for all tasks to finish.
This is typically the case when these tasks are independent of each other.

This needs:

  - A task `Pool` (The internal model to keep track of task progress).
  - The `send` port.
  - The `Msg` to be called when the task completes.
  - Your `List ConcurrentTask` to be run.

Make sure to update your `Model` and pass in the commands returned from `attempt`. e.g. in a branch of `update`:

    let
        ( tasks, cmds ) =
            ConcurrentTask.attemptEach
                { send = send
                , pool = model.pool
                , onComplete = OnComplete
                }
                myTasks
    in
    ( { model | tasks = tasks }, Cmd.batch cmds )

-}
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


{-| Lift a failed task into a successful one reporting the error with a `Result` type.

The main use case for this function is to get more localised errors.
Typically, your `Msg` type will only contain a single pair of `OnProgress` and `OnComplete` variants.

    type Msg
        = ...
        | OnProgress ( ConcurrentTask.Pool Msg TaskError TaskCompleted, Cmd Msg )
        | OnComplete (ConcurrentTask.Response TaskError TaskCompleted)

    type TaskCompleted
        = GotThingOne ThingOne
        | GotThingTwo ThingTwo

In the above situation, you would handle all errors in the same branch of your update function:

    case response of
        ConcurrentTask.error error -> ... -- handle all errors
        ConcurrentTask.Success ... -> ... -- handle successes

However, if instead of running `myTask`, you run `ConcurrentTask.toResult myTask`
and adjust your `TaskCompleted` type to handle results, you obtain errors that are local to the task.

    type TaskCompleted
        = GotThingOne (Result ErrorOne ThingOne)
        | GotThingTwo (Result ErrorTwo ThingTwo)

This pattern makes it easier to handle potential failures at the same place where you would handle correct answers.
It also mirrors the behavior of the elm/http library where each message variant handles its own errors.

    -- Example from elm/http
    type Msg
        = GotBook (Result Http.Error String)
        | GotItems (Result Http.Error (List String))

    -- ConcurrentTask example of handling errors lifted to a task Result.
    case response of
        ConcurrentTask.error error -> ... -- handle non-lifted errors
        ConcurrentTask.Success (GotThingOne result) ->
            -- deal with the first task result
        ConcurrentTask.Success (GotThingTwo result) ->
            -- deal with the second task result

-}
toResult : ConcurrentTask err a -> ConcurrentTask x (Result err a)
toResult task =
    map Ok task
        |> onError (\err -> succeed <| Err err)
