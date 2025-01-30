module Api exposing (ActiveProposal, ApiProvider, DrepInfo, IpfsAnswer(..), IpfsFile, ProposalMetadata, ProtocolParams, ScriptInfo, defaultApiProvider)

import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano.Address exposing (Credential(..), CredentialHash)
import Cardano.Gov exposing (ActionId, CostModels)
import Cardano.Pool as Pool
import Cardano.Script as Script exposing (PlutusVersion(..), Script)
import Cardano.Transaction as Transaction exposing (Transaction)
import Cardano.Utxo exposing (TransactionId)
import Dict exposing (Dict)
import File exposing (File)
import Http
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE
import List.Extra
import Natural exposing (Natural)
import RemoteData exposing (WebData)


type alias ApiProvider msg =
    { loadProtocolParams : (Result Http.Error ProtocolParams -> msg) -> Cmd msg
    , loadGovProposals : (Result Http.Error (List ActiveProposal) -> msg) -> Cmd msg
    , retrieveTxs : List (Bytes TransactionId) -> (Result Http.Error (Dict String Transaction) -> msg) -> Cmd msg
    , getScriptInfo : Bytes CredentialHash -> (Result Http.Error ScriptInfo -> msg) -> Cmd msg
    , getDrepInfo : Credential -> (Result Http.Error DrepInfo -> msg) -> Cmd msg
    , getPoolLiveStake : Bytes Pool.Id -> (Result Http.Error { pool : Bytes Pool.Id, stake : Int } -> msg) -> Cmd msg
    , ipfsAdd : { rpc : String, headers : List ( String, String ), file : File } -> (Result String IpfsAnswer -> msg) -> Cmd msg
    }



-- Protocol Parameters


type alias ProtocolParams =
    { costModels : CostModels
    , drepDeposit : Natural
    }


protocolParamsDecoder : Decoder ProtocolParams
protocolParamsDecoder =
    JD.map4
        (\v1 v2 v3 drepDeposit ->
            { costModels = CostModels (Just v1) (Just v2) (Just v3)
            , drepDeposit = drepDeposit
            }
        )
        (JD.at [ "result", "plutusCostModels", "plutus:v1" ] <| JD.list JD.int)
        (JD.at [ "result", "plutusCostModels", "plutus:v2" ] <| JD.list JD.int)
        (JD.at [ "result", "plutusCostModels", "plutus:v3" ] <| JD.list JD.int)
        (JD.at [ "result", "delegateRepresentativeDeposit", "ada", "lovelace" ] <| JD.map Natural.fromSafeInt JD.int)



-- Governance Proposals


type alias ActiveProposal =
    { id : ActionId
    , actionType : String
    , metadata : WebData ProposalMetadata
    }


type alias ProposalMetadata =
    { title : String
    , abstract : String
    , rawJson : String
    }


proposalsDecoder : Decoder (List ActiveProposal)
proposalsDecoder =
    JD.field "result" <|
        JD.list <|
            JD.map3 ActiveProposal
                (JD.map2
                    (\id index ->
                        { transactionId = Bytes.fromHexUnchecked id
                        , govActionIndex = index
                        }
                    )
                    (JD.at [ "proposal", "transaction", "id" ] JD.string)
                    (JD.at [ "proposal", "index" ] JD.int)
                )
                (JD.at [ "action", "type" ] JD.string)
                (JD.succeed RemoteData.Loading)



-- Retrieve Txs


koiosTxCborDecoder : Decoder (Dict String Transaction)
koiosTxCborDecoder =
    let
        singleTxDecoder : Decoder ( String, Transaction )
        singleTxDecoder =
            JD.map2 Tuple.pair
                (JD.field "tx_hash" JD.string)
                (JD.field "cbor" JD.string)
                |> JD.andThen
                    (\( hashHex, cborHex ) ->
                        case Bytes.fromHex cborHex |> Maybe.andThen Transaction.deserialize of
                            Just tx ->
                                JD.succeed ( hashHex, tx )

                            Nothing ->
                                JD.fail <| "Failed to deserialize Tx: " ++ cborHex
                    )
    in
    JD.list singleTxDecoder
        |> JD.map Dict.fromList



-- Script Info


type alias ScriptInfo =
    { scriptHash : Bytes CredentialHash
    , script : Script
    , nativeCborEncodingMatchesHash : Maybe Bool
    }


koiosFirstScriptInfoDecoder : Decoder ScriptInfo
koiosFirstScriptInfoDecoder =
    JD.list koiosScriptInfoDecoder
        |> JD.andThen
            (\list ->
                case list of
                    [] ->
                        JD.fail "No script info found"

                    first :: _ ->
                        JD.succeed first
            )


koiosScriptInfoDecoder : Decoder ScriptInfo
koiosScriptInfoDecoder =
    JD.map4
        (\hashHex scriptType maybeNative maybePlutusBytes ->
            case Bytes.fromHex hashHex of
                Nothing ->
                    Err <| "Unable to decode the script hash: " ++ hashHex

                Just hash ->
                    if List.member scriptType [ "multisig", "timelock" ] then
                        case maybeNative of
                            Nothing ->
                                Err "Missing native script in Koios response"

                            Just script ->
                                Ok
                                    { scriptHash = hash
                                    , script = Script.Native script
                                    , nativeCborEncodingMatchesHash = Just <| Script.hash (Script.Native script) == hash
                                    }

                    else
                        let
                            plutusVersion =
                                case scriptType of
                                    "plutusV1" ->
                                        Ok PlutusV1

                                    "plutusV2" ->
                                        Ok PlutusV2

                                    "plutusV3" ->
                                        Ok PlutusV3

                                    _ ->
                                        Err <| "Unknown script type: " ++ scriptType
                        in
                        case ( plutusVersion, maybePlutusBytes |> Maybe.andThen Bytes.fromHex ) of
                            ( Ok version, Just bytes ) ->
                                Ok
                                    { scriptHash = hash
                                    , script = Script.Plutus { version = version, script = bytes }
                                    , nativeCborEncodingMatchesHash = Nothing
                                    }

                            ( Err error, _ ) ->
                                Err error

                            ( _, Nothing ) ->
                                Err <| "Missing (or invalid) script CBOR bytes: " ++ Debug.toString maybePlutusBytes
        )
        (JD.field "script_hash" JD.string)
        (JD.field "type" JD.string)
        (JD.field "value" <| JD.maybe Script.jsonDecodeNativeScript)
        (JD.field "bytes" <| JD.maybe JD.string)
        |> JD.andThen
            (\result ->
                case result of
                    Err error ->
                        JD.fail error

                    Ok info ->
                        JD.succeed info
            )



-- DRep Info


type AnyDrepInfo
    = Abstain
    | NoConfidence
    | Registered DrepInfo


type alias DrepInfo =
    { credential : Credential

    -- TODO: eventually need to change to Natural,
    -- but it’s not possible with regular http requests,
    -- because Elm will call JSON.parse() which will silently
    -- lose precision on integers > 2^53
    , votingPower : Int
    }


ogmiosSpecificDrepInfoDecoder : Credential -> Decoder DrepInfo
ogmiosSpecificDrepInfoDecoder cred =
    let
        extractRegistered anyDrep =
            case anyDrep of
                Registered info ->
                    Just info

                _ ->
                    Nothing
    in
    JD.field "result" (JD.list ogmiosAnyDrepInfoDecoder)
        |> JD.map (List.filterMap extractRegistered)
        |> JD.map (List.Extra.find (\drep -> drep.credential == cred))
        |> JD.andThen
            (\drepFound ->
                case drepFound of
                    Nothing ->
                        JD.fail <| "DRep not in the response: " ++ Debug.toString cred

                    Just drepInfo ->
                        JD.succeed drepInfo
            )


ogmiosAnyDrepInfoDecoder : Decoder AnyDrepInfo
ogmiosAnyDrepInfoDecoder =
    JD.field "type" JD.string
        |> JD.andThen
            (\drepType ->
                case drepType of
                    "abstain" ->
                        JD.succeed Abstain

                    "noConfidence" ->
                        JD.succeed NoConfidence

                    "registered" ->
                        JD.map Registered ogmiosRegisteredDrepInfoDecoder

                    _ ->
                        JD.fail <| "Unknown DRep type: " ++ drepType
            )


ogmiosRegisteredDrepInfoDecoder : Decoder DrepInfo
ogmiosRegisteredDrepInfoDecoder =
    JD.field "from" JD.string
        |> JD.andThen
            (\from ->
                case from of
                    "verificationKey" ->
                        JD.succeed VKeyHash

                    "script" ->
                        JD.succeed ScriptHash

                    _ ->
                        JD.fail <| "Unknow credential type: " ++ from
            )
        |> JD.andThen
            (\credType ->
                JD.map2 (\id stake -> { credential = credType id, votingPower = stake })
                    -- TODO: replace fromHexUnchecked with error handling
                    (JD.field "id" JD.string |> JD.map Bytes.fromHexUnchecked)
                    (JD.at [ "stake", "ada", "lovelace" ] JD.int)
            )



-- Pool Live Stake


poolStakeDecoder : Bytes Pool.Id -> Decoder { pool : Bytes Pool.Id, stake : Int }
poolStakeDecoder poolId =
    JD.map (\stake -> { pool = poolId, stake = stake })
        (JD.at [ "result", Pool.toBech32 poolId, "stake", "ada", "lovelace" ] JD.int)



-- IPFS


type IpfsAnswer
    = IpfsError String
    | IpfsAddSuccessful IpfsFile


type alias IpfsFile =
    { name : String
    , cid : String
    , size : String
    }


responseToIpfsAnswer : Http.Response String -> Result String IpfsAnswer
responseToIpfsAnswer response =
    case response of
        Http.GoodStatus_ _ body ->
            JD.decodeString ipfsAnswerDecoder body
                |> Result.mapError JD.errorToString

        Http.BadStatus_ meta body ->
            case JD.decodeString ipfsAnswerDecoder body of
                Ok answer ->
                    Ok answer

                Err _ ->
                    Err <| "Bad status (" ++ String.fromInt meta.statusCode ++ "): " ++ meta.statusText

        Http.NetworkError_ ->
            Err "Network error. Maybe you lost your connection, or some other network error occured."

        Http.Timeout_ ->
            Err "The Pin request timed out."

        Http.BadUrl_ str ->
            Err <| "Incorrect URL: " ++ str


ipfsAnswerDecoder : JD.Decoder IpfsAnswer
ipfsAnswerDecoder =
    JD.oneOf
        -- Error
        [ JD.map3 (\err msg code -> IpfsError <| String.fromInt code ++ " (" ++ err ++ "): " ++ msg)
            (JD.field "error" JD.string)
            (JD.field "message" JD.string)
            (JD.field "status_code" JD.int)
        , JD.map IpfsError
            (JD.field "detail" JD.string)
        , JD.map (\json -> IpfsError <| JE.encode 2 json)
            (JD.field "detail" JD.value)

        -- Blockfrost format
        , JD.map3 (\name hash size -> IpfsAddSuccessful <| IpfsFile name hash size)
            (JD.field "name" JD.string)
            (JD.field "ipfs_hash" JD.string)
            (JD.field "size" JD.string)

        -- CF format
        , JD.map3 (\name hash size -> IpfsAddSuccessful <| IpfsFile name hash size)
            (JD.field "Name" JD.string)
            (JD.field "Hash" JD.string)
            (JD.field "Size" JD.string)
        ]



-- Default API Provider


defaultApiProvider : ApiProvider msg
defaultApiProvider =
    -- Get protocol parameters via Koios
    { loadProtocolParams =
        \toMsg ->
            Http.post
                { url = "https://preview.koios.rest/api/v1/ogmios"
                , body =
                    Http.jsonBody
                        (JE.object
                            [ ( "jsonrpc", JE.string "2.0" )
                            , ( "method", JE.string "queryLedgerState/protocolParameters" )
                            ]
                        )
                , expect = Http.expectJson toMsg protocolParamsDecoder
                }

    -- Get governance proposals via Koios
    , loadGovProposals =
        \toMsg ->
            Http.post
                { url = "https://preview.koios.rest/api/v1/ogmios"
                , body =
                    Http.jsonBody
                        (JE.object
                            [ ( "jsonrpc", JE.string "2.0" )
                            , ( "method", JE.string "queryLedgerState/governanceProposals" )
                            ]
                        )
                , expect = Http.expectJson toMsg proposalsDecoder
                }

    -- Retrieve transactions via Koios by proxying with the server (to avoid CORS errors)
    , retrieveTxs =
        \txIds toMsg ->
            Http.post
                { url = "/proxy/json"
                , body =
                    Http.jsonBody
                        (JE.object
                            [ ( "url", JE.string "https://preview.koios.rest/api/v1/tx_cbor" )
                            , ( "method", JE.string "POST" )
                            , ( "body", JE.object [ ( "_tx_hashes", JE.list (JE.string << Bytes.toHex) txIds ) ] )
                            ]
                        )
                , expect = Http.expectJson toMsg koiosTxCborDecoder
                }

    -- Retrieve script info via Koios by proxying with the server (to avoid CORS errors)
    , getScriptInfo =
        \scriptHash toMsg ->
            Http.post
                { url = "/proxy/json"
                , body =
                    Http.jsonBody
                        (JE.object
                            -- [ ( "url", JE.string "https://api.koios.rest/api/v1/script_info" )
                            [ ( "url", JE.string "https://preview.koios.rest/api/v1/script_info" )
                            , ( "method", JE.string "POST" )
                            , ( "body", JE.object [ ( "_script_hashes", JE.list (JE.string << Bytes.toHex) [ scriptHash ] ) ] )
                            ]
                        )
                , expect = Http.expectJson toMsg koiosFirstScriptInfoDecoder
                }

    -- Retrieve DRep info via Koios using an Ogmios endpoint
    , getDrepInfo =
        \cred toMsg ->
            Http.post
                { url = "https://preview.koios.rest/api/v1/ogmios"
                , body =
                    Http.jsonBody
                        (JE.object
                            [ ( "jsonrpc", JE.string "2.0" )
                            , ( "method", JE.string "queryLedgerState/delegateRepresentatives" )
                            , ( "params"
                              , case cred of
                                    VKeyHash hash ->
                                        JE.object [ ( "keys", JE.list JE.string <| [ Bytes.toHex hash ] ) ]

                                    ScriptHash hash ->
                                        JE.object [ ( "scripts", JE.list JE.string <| [ Bytes.toHex hash ] ) ]
                              )
                            ]
                        )
                , expect = Http.expectJson toMsg (ogmiosSpecificDrepInfoDecoder cred)
                }

    -- Retrieve Pool live stake (end of previous epoch)
    , getPoolLiveStake =
        \poolId toMsg ->
            Http.post
                { url = "https://preview.koios.rest/api/v1/ogmios"
                , body =
                    Http.jsonBody
                        (JE.object
                            [ ( "jsonrpc", JE.string "2.0" )
                            , ( "method", JE.string "queryLedgerState/stakePools" )
                            , ( "params"
                              , JE.object
                                    [ ( "includeStake", JE.bool True )
                                    , ( "stakePools"
                                      , JE.list
                                            (\p -> JE.object [ ( "id", JE.string <| Bytes.toHex p ) ])
                                            [ poolId ]
                                      )
                                    ]
                              )
                            ]
                        )
                , expect = Http.expectJson toMsg (poolStakeDecoder poolId)
                }

    -- Make a request to an IPFS RPC
    , ipfsAdd =
        \{ rpc, headers, file } toMsg ->
            Http.request
                { method = "POST"
                , headers = List.map (\( k, v ) -> Http.header k v) headers
                , url = rpc ++ "/add"
                , body = Http.multipartBody [ Http.filePart "file" file ]
                , expect = Http.expectStringResponse toMsg responseToIpfsAnswer
                , timeout = Nothing
                , tracker = Nothing
                }
    }
