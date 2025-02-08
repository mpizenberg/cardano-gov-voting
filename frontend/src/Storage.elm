module Storage exposing (cacheWrap)

import ConcurrentTask exposing (ConcurrentTask)
import Json.Decode exposing (Decoder)
import Json.Encode as JE exposing (Value)


read : { storeName : String } -> Decoder data -> { key : String } -> ConcurrentTask String data
read { storeName } decoder { key } =
    ConcurrentTask.define
        { function = "storage:read"
        , expect = ConcurrentTask.expectJson decoder
        , errors = ConcurrentTask.expectThrows identity
        , args = JE.object [ ( "storeName", JE.string storeName ), ( "key", JE.string key ) ]
        }
        |> ConcurrentTask.onResponseDecoderFailure (\_ -> ConcurrentTask.fail "not in storage")


write : { storeName : String } -> (data -> Value) -> { key : String } -> data -> ConcurrentTask x ()
write { storeName } encode { key } data =
    ConcurrentTask.define
        { function = "storage:write"
        , expect = ConcurrentTask.expectWhatever
        , errors = ConcurrentTask.expectNoErrors
        , args = JE.object [ ( "storeName", JE.string storeName ), ( "key", JE.string key ), ( "data", encode data ) ]
        }


cacheWrap : { storeName : String } -> Decoder data -> (data -> Value) -> { key : String } -> ConcurrentTask String data -> ConcurrentTask String data
cacheWrap storeName decoder encode key task =
    read storeName decoder key
        |> ConcurrentTask.onError
            -- If the read fails, do the wrapped task
            (\_ ->
                task
                    -- If that task succeeds, write the result to cache
                    |> ConcurrentTask.andThen
                        (\data ->
                            write storeName encode key data
                                |> ConcurrentTask.andThenDo (ConcurrentTask.succeed data)
                        )
            )
