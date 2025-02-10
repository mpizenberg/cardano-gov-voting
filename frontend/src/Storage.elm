module Storage exposing (cacheWrap)

import ConcurrentTask exposing (ConcurrentTask)
import Json.Decode exposing (Decoder)
import Json.Encode as JE exposing (Value)



-- In theory, I’d like to be able to initialize an IndexedDB instance directly from Elm.
-- However, to avoid having a "Maybe Value" for that DB in the Model,
-- it would require that the Model starts in some kind of InitializationModel,
-- and then switch to a full Model after we have retrieved the DB Value.
-- To avoid having to change quite a lot, I’ll just do the DB initialization
-- with the store names directly in the index.html and pass it in the flags of the init function.
--
-- init : { dbName : String, version : Int, storeNames : List String } -> ConcurrentTask String Value
-- init { dbName, version, storeNames } =
--     ConcurrentTask.define
--         { function = "storage:init"
--         , expect = ConcurrentTask.expectJson JD.value
--         , errors = ConcurrentTask.expectThrows identity
--         , args =
--             JE.object
--                 [ ( "dbName", JE.string dbName )
--                 , ( "version", JE.int version )
--                 , ( "storeNames", JE.list JE.string storeNames )
--                 ]
--         }


read : { db : Value, storeName : String } -> Decoder data -> { key : String } -> ConcurrentTask String data
read { db, storeName } decoder { key } =
    ConcurrentTask.define
        { function = "storage:read"
        , expect = ConcurrentTask.expectJson decoder
        , errors = ConcurrentTask.expectThrows identity
        , args =
            JE.object
                [ ( "db", db )
                , ( "storeName", JE.string storeName )
                , ( "key", JE.string key )
                ]
        }
        |> ConcurrentTask.onResponseDecoderFailure (\_ -> ConcurrentTask.fail "not in storage")


write : { db : Value, storeName : String } -> (data -> Value) -> { key : String } -> data -> ConcurrentTask x ()
write { db, storeName } encode { key } data =
    ConcurrentTask.define
        { function = "storage:write"
        , expect = ConcurrentTask.expectWhatever
        , errors = ConcurrentTask.expectNoErrors
        , args =
            JE.object
                [ ( "db", db )
                , ( "storeName", JE.string storeName )
                , ( "key", JE.string key )
                , ( "data", encode data )
                ]
        }


cacheWrap : { db : Value, storeName : String } -> Decoder data -> (data -> Value) -> { key : String } -> ConcurrentTask x data -> ConcurrentTask x data
cacheWrap store decoder encode key task =
    read store decoder key
        |> ConcurrentTask.onError
            -- If the read fails, do the wrapped task
            (\_ ->
                task
                    -- If that task succeeds, write the result to cache
                    |> ConcurrentTask.andThen
                        (\data ->
                            write store encode key data
                                |> ConcurrentTask.andThenDo (ConcurrentTask.succeed data)
                        )
            )
